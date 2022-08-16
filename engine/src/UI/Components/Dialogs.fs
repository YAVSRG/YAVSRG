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

    abstract member Close : unit -> unit
    default this.Close() = closed <- true

module Dialog =
    
    let mutable private current : Dialog option = None
    
    let fade = Animation.Fade 0.0f

    type Display() =
        inherit Overlay(NodeType.None)

        override this.Draw() =
            if fade.Value > 0.0f then
                Draw.rect this.Bounds (Color.FromArgb(int (200f * fade.Value), Color.Black))
                match current with
                | Some d -> d.Draw()
                | None -> ()

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            fade.Update elapsedTime
            match current with
            | Some d ->
                d.Update(elapsedTime, moved)
                if d.Closed then
                    current <- None
                    fade.Target <- 0.0f
                else Input.finish_frame_events()
            | None -> ()

    let display = Display()

    let show (d: Dialog) =
        match current with
        | Some existing -> failwithf "Already showing %O. Nested dialogs not supported" existing
        | None -> current <- Some d; d.Init display; fade.Target <- 1.0f

    let exists() = current.IsSome

type Dialog with member this.Show() = Dialog.show this