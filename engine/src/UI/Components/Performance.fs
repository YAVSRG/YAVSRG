namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

type PerformanceMonitor() =
    inherit StaticWidget(NodeType.None)

    // todo: just use Render.FPS
    let mutable frame_times = []
    let mutable fps = 0.0

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        frame_times <- elapsedTime :: frame_times
        if frame_times.Length > 600 then frame_times <- List.take 600 frame_times
        fps <- min 10000 (fps * 0.99 + (10.0 / List.head frame_times))

    override this.Draw() =
        let mutable x = this.Bounds.Left
        let step = this.Bounds.Width / 600.0f
        for t in frame_times do
            Draw.rect(Rect.Create(x, this.Bounds.Bottom - float32 t, x + step, this.Bounds.Bottom)) Color.Red
            x <- x + step
        if frame_times <> [] then
            Text.drawJustB(Style.baseFont, sprintf "%.1f FPS" fps, 30.0f, this.Bounds.Right - 20.0f, this.Bounds.Top + 20.0f, (Color.White, Color.Black), Alignment.RIGHT)