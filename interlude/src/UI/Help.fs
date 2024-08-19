namespace Interlude.UI

open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private HelpInfo =
    {
        Data: Callout
        Size: float32 * float32
        Target: Widget
        Bind: Bind
    }

module HelpOverlay =

    let fade = Animation.Fade(0.0f)
    let mutable private current_info: HelpInfo option = None
    let mutable internal info_available = false

    type private Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            match current_info with
            | None ->
                fade.Target <- 0.0f
            | Some t ->
                fade.Target <- 1.0f
                if t.Bind.Released() then
                    current_info <- None

                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)
                let width, height = t.Size

                let x =
                    outline.CenterX - width * 0.5f
                    |> min (Viewport.bounds.Width - width - 50.0f)
                    |> max 50.0f

                let y =
                    if outline.Top > Viewport.bounds.CenterY then
                        outline.Top - 50.0f - height - 60.0f
                    else
                        outline.Bottom + 50.0f

                let callout_bounds = Rect.Box(x, y, width, height + 60.0f)
                Callout.update (callout_bounds.Left, callout_bounds.Top + 30.0f, width, height, t.Data)

            fade.Update elapsed_ms
            info_available <- false

        override this.Draw() =
            match current_info with
            | None ->
                Draw.rect Viewport.bounds (Colors.shadow_2.O3a fade.Alpha)
            | Some t ->
                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)
                let alpha = fade.Alpha

                LoadingAnimation.draw_border_piece outline 0.0f fade.Value (Colors.yellow_accent.O3a alpha)

                // blackout effect
                Draw.rect (Viewport.bounds.SliceL outline.Left) (Colors.shadow_2.O3a alpha)
                Draw.rect (Viewport.bounds.ShrinkL outline.Right) (Colors.shadow_2.O3a alpha)

                Draw.rect
                    (Viewport.bounds
                        .ShrinkL(outline.Left)
                        .SliceL(outline.Width)
                        .SliceT(outline.Top))
                    (Colors.shadow_2.O3a alpha)

                Draw.rect
                    (Viewport.bounds
                        .ShrinkL(outline.Left)
                        .SliceL(outline.Width)
                        .ShrinkT(outline.Bottom))
                    (Colors.shadow_2.O3a alpha)

                // draw tooltip
                let width, height = t.Size

                let x =
                    outline.CenterX - width * 0.5f
                    |> min (Viewport.bounds.Width - width - 50.0f)
                    |> max 50.0f

                let y =
                    if outline.Top > Viewport.bounds.CenterY then
                        outline.Top - 50.0f - height
                    else
                        outline.Bottom + 50.0f

                let callout_bounds = Rect.Box(x, y, width, height)
                Draw.rect callout_bounds Colors.cyan.O2
                let frame_bounds = callout_bounds.Expand(5.0f)
                Draw.rect (frame_bounds.SliceT 5.0f) Colors.cyan_accent
                Draw.rect (frame_bounds.SliceB 5.0f) Colors.cyan_accent
                Draw.rect (frame_bounds.SliceL 5.0f) Colors.cyan_accent
                Draw.rect (frame_bounds.SliceR 5.0f) Colors.cyan_accent

                Callout.draw (
                    callout_bounds.Left,
                    callout_bounds.Top,
                    width,
                    height,
                    Colors.text,
                    t.Data
                )

    let display : Widget = Display()

    let show (b: Bind, w: Widget, body: Callout) =
        let t: HelpInfo =
            {
                Data = body
                Size = Callout.measure body
                Target = w
                Bind = b
            }

        current_info <- Some t

    let available () =
        info_available <- true


type Help(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let content = content.Icon(Icons.INFO)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.hover this.Bounds then
            HelpOverlay.available()

            if (%%"tooltip").Tapped() then
                HelpOverlay.show ((%%"tooltip"), this, content)

    override this.Draw() = ()

    static member Info(feature: string) =
        Callout.Normal
            .Title(%feature)
            .Body(%(sprintf "%s.tooltip" feature))

    static member Info(feature: string, hotkey: Hotkey) =
        Callout.Normal
            .Title(%feature)
            .Body(%(sprintf "%s.tooltip" feature))
            .Hotkey(hotkey)

[<AutoOpen>]
module Help =

    type Container with
        member this.Help(content: Callout) = this |+ Help(content)