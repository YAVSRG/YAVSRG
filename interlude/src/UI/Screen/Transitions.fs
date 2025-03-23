namespace Interlude.UI

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Interlude.UI
open Interlude.Options

module Transitions =

    type Transition =
        | Default
        | UnderLogo
        | EnterGameplayNoFadeAudio
        | EnterGameplayFadeAudio
        | LeaveGameplay
        member this.Duration =
            match this with
            | Default
            | UnderLogo -> 500.0
            | EnterGameplayNoFadeAudio
            | EnterGameplayFadeAudio
            | LeaveGameplay -> 350.0

    let private fancy_transition (inbound: bool) (amount: float32) (bounds: Rect) =
        let amount = if inbound then amount else 2.0f - amount
        let a = int (255.0f * (1.0f - Math.Abs(amount - 1.0f)))
        Wedge.variant_2 0.0f 1111.0f (Color.FromArgb(a, 0, 160, 255)) 0.0f 1.8f amount
        Wedge.variant_2 0.0f 1111.0f (Color.FromArgb(a, 0, 200, 255)) 0.1f 1.9f amount
        Wedge.variant_2 0.0f 1111.0f (Color.FromArgb(a, 0, 240, 255)) 0.2f 2.0f amount
        Wedge.variant_1 300.0f 500.0f Color.White 0.5f 1.5f amount
        Bubble.draw (400.0f, 200.0f) 100.0f 150.0f Color.White 1.2f 1.5f amount
        Bubble.draw (300.0f, 250.0f) 60.0f 90.0f Color.White 1.3f 1.6f amount
        Bubble.draw (1600.0f, 600.0f) 80.0f 120.0f Color.White 1.0f 1.3f amount
        Bubble.draw (1400.0f, 700.0f) 50.0f 75.0f Color.White 1.4f 1.7f amount

    let private draw_internal (t: Transition) (inbound: bool) (amount: float32) (bounds: Rect) =
        Render.stencil_create false
        match t with
        | Default
        | UnderLogo ->
            DiamondsWipe.draw inbound amount bounds
            Render.stencil_begin_draw ()
            Background.draw (bounds, Palette.color (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)

        | EnterGameplayNoFadeAudio
        | EnterGameplayFadeAudio ->
            TriangleWipe.draw_downward inbound amount bounds
            Render.stencil_begin_draw ()
            Background.draw (bounds, Palette.color (255.0f * amount |> int, 0.3f, 0.5f), 1.0f)

        | LeaveGameplay ->
            TriangleWipe.draw_upward inbound amount bounds
            Render.stencil_begin_draw ()
            Background.draw (bounds, Palette.color (255.0f * amount |> int, 0.3f, 0.5f), 1.0f)

        Render.stencil_finish ()

    let private in_timer = Animation.Delay 500.0
    let private out_timer = Animation.Delay 500.0

    let mutable current : Transition option = None

    let in_progress () = current.IsSome

    let draw (transition_type: Transition) (bounds: Rect) : unit =
        let inbound = not in_timer.Complete

        let amount =
            Math.Clamp(
                (if inbound then in_timer.Progress else 1.0 - out_timer.Progress),
                0.0,
                1.0
            )

        if transition_type = EnterGameplayFadeAudio then
            Audio.change_volume (options.AudioVolume.Value * (1.0 - amount), options.AudioVolume.Value * (1.0 - amount))

        draw_internal transition_type inbound (float32 amount) (bounds.Expand 1.0f)

    let animate (func: unit -> unit, transition_type: Transition) : Animation =

        match current with
        | Some _ ->
            failwith "Should not be called while a transition is already in progress"
        | None ->

        current <- Some transition_type
        in_timer.Interval <- transition_type.Duration
        out_timer.Interval <- transition_type.Duration

        Animation.seq
            [
                in_timer
                Animation.Action(fun () ->
                    func ()
                    out_timer.FrameSkip()
                )
                out_timer
                Animation.Action(fun () ->
                    in_timer.Reset()
                    out_timer.Reset()
                    current <- None
                    if transition_type = EnterGameplayFadeAudio then Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
                )
            ]