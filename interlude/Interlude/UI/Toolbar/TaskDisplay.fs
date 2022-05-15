namespace Interlude.UI.Toolbar

open System
open System.Threading.Tasks
open Prelude.Common
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.Utils

module TaskDisplay =

    type TaskBox(task: BackgroundTask.ManagedTask) as this =
        inherit Widget()

        let fade = AnimationFade(0.0f, Target = 1.0f)
        let color = AnimationColorMixer(Color.White)

        let mutable closing = false

        let close() =
            closing <- true
            Animation.Serial(
                AnimationTimer 2000.0,
                AnimationAction (fun () -> fade.Target <- 0.0f),
                AnimationTimer 800.0,
                AnimationAction this.Destroy
            ) |> this.Animation.Add

        do
            this.Animation.Add fade
            this.Animation.Add color

            Clickable(
                ( fun () ->
                    if not closing then
                        if task.Status <> TaskStatus.RanToCompletion then
                            Selection.Menu.ConfirmDialog("Cancel this task?", F task.Cancel close).Show()
                        else close()
                ),
                ignore
            ) |> this.Add

            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)

        override this.Draw() =

            let a = 255.0f * fade.Value |> int
            let col = color.GetColor a
            
            Draw.rect (this.Bounds.SliceTop 5.0f) col Sprite.Default
            Draw.rect (this.Bounds.SliceBottom 5.0f) col Sprite.Default
            Draw.rect (this.Bounds.SliceLeft 5.0f) col Sprite.Default
            Draw.rect (this.Bounds.SliceRight 5.0f) col Sprite.Default

            let inner = this.Bounds.Shrink 5.0f
            
            Draw.rect inner (Color.FromArgb(a / 4 * 3, Color.Black)) Sprite.Default
            Draw.rect inner (Color.FromArgb(a / 2, col)) Sprite.Default

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
        inherit SlideDialog(SlideDialog.Direction.Up, Render.vheight)
        do 
            this.Add taskBoxes
            TextBox(K "Background tasks", K (Color.White, Color.Black), 0.5f)
                .Position { Left = 0.5f %- 300.0f; Top = 1.0f %- 980.0f; Right = 0.5f %+ 300.0f; Bottom = 1.0f %- 900.0f }
            |> this.Add

        override this.Draw() =
            Draw.rect taskBoxes.Bounds (Style.accentShade(200, 0.6f, 0.1f)) Sprite.Default
            base.Draw()

        override this.OnClose() = this.Remove taskBoxes
