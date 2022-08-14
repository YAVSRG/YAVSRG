namespace Interlude.UI.Toolbar

open System
open System.Threading.Tasks
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.UI
open Interlude.Utils

// todo: basically redo this whole thing when tasks are redesigned
// todo: does not belong in toolbar folder
module TaskDisplay =

    type TaskBox(task: BackgroundTask.ManagedTask) as this =
        inherit Frame(NodeType.Leaf, Border = (fun () -> this.Color), Fill = fun () -> Color.FromArgb(127, this.Color))

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let color = Animation.Color Color.White

        do
            let textFunc = fun () ->
                let a = fade.Alpha
                (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black))
            this
            |+ Text(task.Name, Align = Alignment.LEFT, Color = textFunc, Position = Position.SliceTop 60.0f)
            |* Text(task.get_Info, Align = Alignment.LEFT, Color = textFunc, Position = Position.SliceBottom 40.0f)

        member private this.Color = color.GetColor fade.Alpha

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

            color.Update(elapsedTime)
            fade.Update(elapsedTime)

    let private tasks = ResizeArray<BackgroundTask.ManagedTask>()

    let init () = BackgroundTask.Subscribe(fun t -> if t.Visible then tasks.Add t)

    type Dialog() = 
        inherit Percyqaz.Flux.UI.Dialog()

        let flow = FlowContainer.Vertical<TaskBox>(100.0f)
        let scroll = ScrollContainer.Flow(flow, Margin = Style.padding, Position = Position.Margin(400.0f, 200.0f))

        do for task in tasks do flow.Add( TaskBox task )

        override this.Draw() =
            Draw.rect scroll.Bounds (Style.color(200, 0.6f, 0.1f))
            Text.drawFillB(Style.baseFont, L"menu.tasks", this.Bounds.SliceTop(180.0f).SliceBottom(80.0f), Style.text(), Alignment.CENTER)
            scroll.Draw()

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            scroll.Update(elapsedTime, moved)
            if Mouse.leftClick() || (!|"exit").Tapped() then
                this.Close()

        override this.Init(parent: Widget) =
            base.Init parent
            scroll.Init this