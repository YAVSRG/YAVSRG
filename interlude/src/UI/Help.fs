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
    }

module HelpOverlay =

    let mutable private current_info: HelpInfo option = None
    let overlay_fade = Animation.Fade 0.0f
    let mutable internal on = false
    //let mutable private info_regions: Rect list = []

    type private Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            on <- (%%"tooltip").Pressed()
            overlay_fade.Target <- if on then 1.0f else 0.0f
            overlay_fade.Update elapsed_ms

            match current_info with
            | None -> ()
            | Some t ->
                if t.Fade.Target = 0.0f || on then
                    t.Fade.Update elapsed_ms

                if t.Fade.Target <> 0.0f then
                    if not (Mouse.hover t.Target.Bounds) || not on then
                        t.Fade.Target <- 0.0f
                elif t.Fade.Value < 0.01f then
                    current_info <- None

                if t.Fade.Value > 0.01f then

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


        override this.Draw() =
            //let a = overlay_fade.Alpha
            //if on then
            //    while not (List.isEmpty info_regions) do

            //        let bounds = info_regions.Head

            //        let w = (max bounds.Width 200.0f) / 2.0f
            //        let bounds : Rect = bounds.SliceCenterX(2.0f * w)

            //        Draw.untextured_quad 
            //            (bounds.SliceLeft(w).AsQuad)
            //            (Quad.gradient_left_to_right (Colors.yellow_accent.O4a 0) (Colors.yellow_accent.O2a a))
            //        Draw.untextured_quad 
            //            (bounds.SliceRight(w).AsQuad)
            //            (Quad.gradient_left_to_right (Colors.yellow_accent.O2a a) (Colors.yellow_accent.O4a 0))

            //        Text.fill_b(Style.font, Icons.INFO, bounds.SliceCenterY(50.0f), (Colors.green_accent.O4a a, Colors.shadow_2.O4a a), 0.5f)

            //        info_regions <- List.tail info_regions

            match current_info with
            | None -> ()
            | Some t ->
                let outline = t.Target.Bounds.Expand(20.0f).Intersect(Viewport.bounds)

                let c l =
                    Math.Clamp((t.Fade.Value - l) / 0.25f, 0.0f, 1.0f)
                // border around thing
                Draw.rect
                    (outline.SliceLeft(Style.PADDING).SliceBottom(outline.Height * c 0.0f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceTop(Style.PADDING).SliceLeft(outline.Width * c 0.25f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceRight(Style.PADDING).SliceTop(outline.Height * c 0.5f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                Draw.rect
                    (outline.SliceBottom(Style.PADDING).SliceRight(outline.Width * c 0.75f))
                    (Colors.yellow_accent.O3a t.Fade.Alpha)

                // blackout effect
                Draw.rect (Viewport.bounds.SliceLeft outline.Left) (Colors.shadow_2.O2a t.Fade.Alpha)
                Draw.rect (Viewport.bounds.TrimLeft outline.Right) (Colors.shadow_2.O2a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .TrimLeft(outline.Left)
                        .SliceLeft(outline.Width)
                        .SliceTop(outline.Top))
                    (Colors.shadow_2.O2a t.Fade.Alpha)

                Draw.rect
                    (Viewport.bounds
                        .TrimLeft(outline.Left)
                        .SliceLeft(outline.Width)
                        .TrimTop(outline.Bottom))
                    (Colors.shadow_2.O2a t.Fade.Alpha)

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
                Draw.rect callout_bounds (Colors.cyan.O3a t.Fade.Alpha)
                let frame_bounds = callout_bounds.Expand(5.0f)
                Draw.rect (frame_bounds.SliceTop 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceBottom 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceLeft 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)
                Draw.rect (frame_bounds.SliceRight 5.0f) (Colors.cyan_accent.O4a t.Fade.Alpha)

                Callout.draw (
                    callout_bounds.Left,
                    callout_bounds.Top,
                    width,
                    height,
                    (Colors.white.O4a t.Fade.Alpha, Colors.shadow_1.O4a t.Fade.Alpha),
                    t.Data
                )

    let display : Widget = Display()

    let show (w: Widget, body: Callout) =
        let t: HelpInfo =
            {
                Data = body
                Size = Callout.measure body
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Target = w
            }

        current_info <- Some t

    //let region (bounds: Rect) = info_regions <- bounds :: info_regions

type Help(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let content = content.Icon(Icons.INFO)
    let mutable hover = false

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        //if HelpOverlay.on then

            //HelpOverlay.region this.Bounds

        if not hover && Mouse.hover this.Bounds then
            hover <- true
            HelpOverlay.show (this, content)

        elif hover && not (Mouse.hover this.Bounds) then
            hover <- false

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

type Tooltip(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let SPACING = 10.0f
    
    let mutable width, height = 0.0f, 0.0f
    let mutable hover = false

    override this.Init(parent) =
        let a, b = Callout.measure content
        width <- a; height <- b
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if not hover && Mouse.hover this.Bounds then
            hover <- true
        elif hover && not (Mouse.hover this.Bounds) then
            hover <- false

    override this.Draw() =
        if not hover then () else

        let x =
            this.Bounds.CenterX - width * 0.5f
            |> min (Viewport.bounds.Width - width - SPACING)
            |> max SPACING

        let y =
            if this.Bounds.Top > Viewport.bounds.CenterY then
                this.Bounds.Top - SPACING - height
            else
                this.Bounds.Bottom + SPACING

        let bounds = Rect.Box(x, y, width, height)

        Draw.rect bounds Colors.shadow_2.O3
        Callout.draw (bounds.Left, bounds.Top, width, height, Colors.text, content)

module Help =
    let text (text: string) = Callout.Normal.Body(text)
    let hotkey (hk: Hotkey) = Callout.Normal.Hotkey(hk)
    let text_hotkey (text: string, hk: Hotkey) = Callout.Normal.Body(text).Hotkey(hk)
    let info (title: string, body: string) = Callout.Normal.Title(title).Body(body)
    let info_hotkey (title: string, body: string, hk: Hotkey) = Callout.Normal.Title(title).Body(body).Hotkey(hk)

[<AutoOpen>]
module HelpExtensions =

    type Container with
        member this.Help(content: Callout) = this |+ Help content
        member this.Tooltip(content: Callout) = this |+ Tooltip content