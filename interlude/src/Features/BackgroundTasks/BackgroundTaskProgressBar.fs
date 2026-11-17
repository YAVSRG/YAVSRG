namespace Interlude.Features.Import

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Data.Imports

type BackgroundTaskProgressBar() =
    inherit StaticWidget(NodeType.None)

    let animation = Animation.Counter(1500.0)
    let fade = Animation.Fade(0.0f)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)
        animation.Update(elapsed_ms)
        fade.Target <- if TaskTracking.in_progress() then 1.0f else 0.0f
        fade.Update(elapsed_ms)

    override this.Draw() : unit =
        let alpha = fade.Alpha

        if alpha > 0 then

            match TaskTracking.current_progress() with
            | InProgress(Downloading p) ->
                Render.rect this.Bounds (Colors.shadow_2.O4a(alpha))
                Render.rect (this.Bounds.SlicePercentL(p)) (Colors.cyan.O4a(alpha))
            | InProgress(Processing(i, total)) ->
                Render.rect this.Bounds (Colors.cyan.O4a(alpha))
                Render.rect (this.Bounds.SlicePercentL(float32 i / float32 total)) (Colors.green_accent.O4a(alpha))
            | InProgress(Nested(_, i, total, _)) ->
                Render.rect this.Bounds (Colors.cyan.O4a(alpha))
                Render.rect (this.Bounds.SlicePercentL(float32 i / float32 total)) (Colors.green_accent.O4a(alpha))
            | InProgress(Generic _) ->
                let tick_width = this.Bounds.Width * 0.2f

                let pos =
                    -tick_width + (this.Bounds.Width + tick_width) * float32 animation.Progress

                Render.rect_edges
                    (this.Bounds.Left + max 0.0f pos)
                    this.Bounds.Top
                    (this.Bounds.Left + min this.Bounds.Width (pos + tick_width))
                    this.Bounds.Bottom
                    (Colors.white.O4a(alpha))
            | Completed -> Render.rect this.Bounds (Colors.green_accent.O4a(alpha))
            | Faulted -> Render.rect this.Bounds (Colors.red_accent.O4a(alpha))
