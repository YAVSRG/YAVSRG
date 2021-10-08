namespace Interlude.UI.Toolbar

open System
open System.Drawing
open Prelude.Common
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils

module TaskDisplay =

    let private taskBox (t: BackgroundTask.ManagedTask) = 
        let w = Frame()

        TextBox(t.get_Name, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)
        |> w.Add

        TextBox(t.get_Info, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(0.0f, 0.0f, 50.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
        |> w.Add

        Clickable(
            (fun () ->
                match t.Status with
                | Threading.Tasks.TaskStatus.RanToCompletion -> w.Destroy()
                | _ -> t.Cancel(); w.Destroy()), ignore)
        |> w.Add

        w |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 90.0f, 0.0f)

    let private taskBoxes =
        let f = FlowContainer()
        BackgroundTask.Subscribe(fun t -> if t.Visible then f.Add(taskBox t))
        f |> positionWidget(-500.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)

    let init () = BackgroundTask.Subscribe(fun t -> if t.Visible then taskBoxes.Add(taskBox t))

    type Dialog() as this = 
        inherit SlideDialog(SlideDialog.Direction.Left, 500.0f)
        do this.Add taskBoxes

        override this.Draw() =
            Draw.rect taskBoxes.Bounds (Style.accentShade(180, 0.4f, 0.0f)) Sprite.Default
            base.Draw()

        override this.OnClose() = this.Remove taskBoxes
