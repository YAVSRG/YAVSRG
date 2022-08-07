namespace Interlude.UI.Toolbar

open System
open System.Threading.Tasks
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils

module TaskDisplay =

    type TaskBox(task: BackgroundTask.ManagedTask) as this =
        inherit Widget1()

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let color = Animation.Color Color.White

        let mutable closing = false

        let close() =
            closing <- true
            Animation.seq [
                Animation.Delay 2000.0 :> Animation;
                Animation.Action (fun () -> fade.Target <- 0.0f);
                Animation.Delay 800.0;
                Animation.Action this.Destroy
            ] |> this.Animation.Add

        do
            this.Animation.Add fade
            this.Animation.Add color

            Clickable(
                ( fun () ->
                    if not closing then
                        if task.Status <> TaskStatus.RanToCompletion then ()
                            //Selection.Menu.ConfirmDialog("Cancel this task?", F task.Cancel close).Show()
                        else close()
                ),
                ignore
            ) |> this.Add

            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)

        override this.Draw() =

            let a = fade.Alpha
            let col = color.GetColor a
            
            Draw.rect (this.Bounds.SliceTop 5.0f) col
            Draw.rect (this.Bounds.SliceBottom 5.0f) col
            Draw.rect (this.Bounds.SliceLeft 5.0f) col
            Draw.rect (this.Bounds.SliceRight 5.0f) col

            let inner = this.Bounds.Shrink 5.0f
            
            Draw.rect inner (Color.FromArgb(a / 4 * 3, Color.Black))
            Draw.rect inner (Color.FromArgb(a / 2, col))

            Text.drawFillB(Interlude.Content.font, task.Name, inner.SliceTop 60.0f, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)), 0.0f)
            Text.drawFillB(Interlude.Content.font, task.Info, inner.SliceBottom 40.0f, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)), 0.0f)

        override this.Update(elapsedTime, bounds) =

            base.Update(elapsedTime, bounds)

            color.SetColor(
                match task.Status with
                | TaskStatus.Faulted -> Color.Red
                | TaskStatus.RanToCompletion -> Color.Green
                | TaskStatus.Created
                | TaskStatus.WaitingForActivation
                | TaskStatus.WaitingForChildrenToComplete
                | TaskStatus.WaitingToRun -> Color.LightBlue
                | TaskStatus.Running -> Color.Blue
                | TaskStatus.Canceled
                | _ -> Color.Gray
            )

            if not closing && task.Status = TaskStatus.RanToCompletion then close()

    let private taskBoxes = FlowContainer().Position { Left = 0.5f %- 300.0f; Top = 1.0f %- 900.0f; Right = 0.5f %+ 300.0f; Bottom = 1.0f %- 100.0f }

    let init () = BackgroundTask.Subscribe(fun t -> if t.Visible then taskBoxes.Add(TaskBox t))

    type Dialog() as this = 
        inherit SlideDialog(SlideDialog.Direction.Up, Viewport.vheight)
        do 
            this.Add taskBoxes
            TextBox(K "Background tasks", K (Color.White, Color.Black), 0.5f)
                .Position { Left = 0.5f %- 300.0f; Top = 1.0f %- 980.0f; Right = 0.5f %+ 300.0f; Bottom = 1.0f %- 900.0f }
            |> this.Add

        override this.Draw() =
            Draw.rect taskBoxes.Bounds (Style.color(200, 0.6f, 0.1f))
            base.Draw()

        override this.OnClose() = this.Remove taskBoxes
