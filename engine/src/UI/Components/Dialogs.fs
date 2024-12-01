namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

[<AbstractClass>]
type Dialog() =
    inherit Overlay(NodeType.None)

    let mutable closed = false
    member this.Closed = closed

    abstract member Close: unit -> unit
    default this.Close() = closed <- true

module Dialog =

    let mutable private current: Dialog option = None

    let fade = Animation.Fade 0.0f

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Draw() =
            if fade.Value > 0.0f then
                Render.rect this.Bounds (Color.FromArgb(int (225f * fade.Value), 35, 35, 35))

                match current with
                | Some d -> d.Draw()
                | None -> ()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            fade.Update elapsed_ms

            match current with
            | Some d ->
                d.Update(elapsed_ms, moved)

                if d.Closed then
                    current <- None
                    fade.Target <- 0.0f
                else
                    Input.finish_frame_events ()
            | None -> ()

    let display = Display()

    let show (d: Dialog) =
        match current with
        | Some existing ->
            d.Init display
            d.Close()
        | None ->
            current <- Some d
            d.Init display
            fade.Target <- 1.0f

    let exists () = current.IsSome

    let close () =
        match current with
        | Some d -> d.Close()
        | None -> ()

type Dialog with
    member this.Show() = Dialog.show this
