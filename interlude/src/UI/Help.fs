namespace Interlude.UI

open System
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private HelpInfo =
    {
        Data: Callout
        Size: float32 * float32
        Fade: Animation.Fade
        Target: Widget
        Bind: Bind
    }

module HelpOverlay =

    let mutable private current_info: HelpInfo option = None
    let mutable internal info_available = false

    type private Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            match current_info with
            | None -> ()
            | Some t ->
                t.Fade.Update elapsed_ms

                if t.Fade.Target <> 0.0f then
                    if t.Bind.Released() then
                        t.Fade.Target <- 0.0f
                elif t.Fade.Value < 0.01f then
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

            info_available <- false

        override this.Draw() =
            match current_info with
            | None -> ()
            | Some t ->
                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)

                let c l =
                    Math.Clamp((t.Fade.Value - l) / 0.25f, 0.0f, 1.0f)
                // border around thing
                Draw.rect
                    (outline.SliceL(Style.PADDING).SliceB(outline.Height * c 0.0f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceT(Style.PADDING).SliceL(outline.Width * c 0.25f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceR(Style.PADDING).SliceT(outline.Height * c 0.5f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceB(Style.PADDING).SliceR(outline.Width * c 0.75f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                // blackout effect
                Draw.rect (Viewport.bounds.SliceL outline.Left) (Colors.shadow_2.O3a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.ShrinkL outline.Right) (Colors.shadow_2.O3a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .ShrinkL(outline.Left)
                        .SliceL(outline.Width)
                        .SliceT(outline.Top))
                    (Colors.shadow_2.O3a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .ShrinkL(outline.Left)
                        .SliceL(outline.Width)
                        .ShrinkT(outline.Bottom))
                    (Colors.shadow_2.O3a t.Fade.Alpha)

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
                Draw.rect callout_bounds (Colors.cyan.O2a t.Fade.Alpha)
                let frame_bounds = callout_bounds.Expand(5.0f)
                Draw.rect (frame_bounds.SliceT 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceB 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceL 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceR 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)

                Callout.draw (
                    callout_bounds.Left,
                    callout_bounds.Top,
                    width,
                    height,
                    (Colors.white.O4a t.Fade.Alpha, Colors.shadow_1.O4a t.Fade.Alpha),
                    t.Data
                )

    let display : Widget = Display()

    let show (b: Bind, w: Widget, body: Callout) =
        let t: HelpInfo =
            {
                Data = body
                Size = Callout.measure body
                Fade = Animation.Fade(0.0f, Target = 1.0f)
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