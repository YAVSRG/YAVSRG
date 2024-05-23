namespace Interlude.UI

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

module Transitions =

    let private DURATION = 500.0

    let private in_timer = Animation.Delay DURATION
    let private out_timer = Animation.Delay DURATION

    type Flags =
        | Default = 0
        | UnderLogo = 1
        | GiveUp = 2

    let mutable flags = Flags.Default
    let mutable active = false

    let private fancy_transition inbound amount bounds =
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

    let private draw_internal flags inbound amount bounds =
        Stencil.start_stencilling false
        DiamondWipe.draw inbound amount bounds
        Stencil.start_drawing ()
        Background.draw (bounds, Palette.color (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
        Stencil.finish ()
        if flags &&& Flags.GiveUp = Flags.GiveUp then
            Text.fill_b(Style.font, "Gave up", bounds.Shrink(bounds.Width * 0.25f, bounds.Height * 0.25f), Colors.text_red_2, 0.5f)

    // setup

    let draw (bounds: Rect) =
        if active then
            let inbound = in_timer.Elapsed < DURATION

            let amount =
                Math.Clamp(
                    (if inbound then
                         in_timer.Elapsed / DURATION
                     else
                         1.0 - (out_timer.Elapsed / DURATION)),
                    0.0,
                    1.0
                )
                |> float32

            draw_internal flags inbound amount bounds

    let animate (func: unit -> unit, _flags: Flags) : Animation =
        if active then
            failwith "Should not be called while a transition is already in progress"

        active <- true
        flags <- _flags

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
                    active <- false
                )
            ]
