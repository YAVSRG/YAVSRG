namespace Interlude.UI

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude

module LoadingIndicator =

    type Strip(is_loading: unit -> bool) =
        inherit StaticWidget(NodeType.None)

        let animation = Animation.Counter(1500.0)
        let fade = Animation.Fade 0.0f

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            animation.Update elapsed_ms
            fade.Target <- if is_loading () then 1.0f else 0.0f
            fade.Update elapsed_ms

        override this.Draw() =
            if fade.Alpha = 0 then
                ()
            else

                let tick_width = this.Bounds.Width * 0.2f

                let pos =
                    -tick_width
                    + (this.Bounds.Width + tick_width) * float32 animation.Progress

                Render.rect_edges
                    (this.Bounds.Left + max 0.0f pos)
                    this.Bounds.Top
                    (this.Bounds.Left + min this.Bounds.Width (pos + tick_width))
                    this.Bounds.Bottom
                    (Colors.white.O4a fade.Alpha)

    type Border(is_loading: unit -> bool) =
        inherit StaticWidget(NodeType.None)

        let animation = Animation.Counter(1500.0)
        let fade = Animation.Fade 0.0f

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            animation.Update elapsed_ms
            fade.Target <- if is_loading () then 1.0f else 0.0f
            fade.Update elapsed_ms

        override this.Draw() =
            if fade.Alpha = 0 then
                ()
            else

                let b = this.Bounds.Expand(Style.PADDING)
                LoadingAnimation.draw_border b (float32 animation.Progress) (Colors.white.O4a fade.Alpha)

type WIP() as this =
    inherit StaticWidget(NodeType.None)

    let text = %"misc.wip"

    do this.Position <- Position.SliceB(100.0f)

    override this.Draw() =
        Render.rect this.Bounds (Color.FromArgb(127, Color.Yellow))
        let w = this.Bounds.Width / 20.0f

        for i = 0 to 19 do
            Render.rect_size
                (this.Bounds.Left + w * float32 i)
                this.Bounds.Top
                w
                10.0f
                (if i % 2 = 0 then Color.Yellow else Color.Black)

            Render.rect_size
                (this.Bounds.Left + w * float32 i)
                (this.Bounds.Bottom - 10.0f)
                w
                10.0f
                (if i % 2 = 1 then Color.Yellow else Color.Black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(20.0f), Colors.text, Alignment.CENTER)

// todo: give empty states an optional action
type EmptyState(icon: string, text: string) =
    inherit StaticWidget(NodeType.None)

    member val Subtitle = "" with get, set

    override this.Draw() =
        Text.fill_b (Style.font, icon, this.Bounds.Shrink(30.0f, 100.0f).SliceT(200.0f), Colors.text_greyout, Alignment.CENTER)

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.Shrink(30.0f, 100.0f).ShrinkT(175.0f).SliceT(60.0f),
            Colors.text_greyout,
            Alignment.CENTER
        )

        Text.fill_b (
            Style.font,
            this.Subtitle,
            this.Bounds.Shrink(30.0f, 100.0f).ShrinkT(230.0f).SliceT(40.0f),
            Colors.text_greyout,
            Alignment.CENTER
        )

// todo: perhaps bin this in favour of the loading indicators which are much better
// OR add the loading indicator to this and it will look good
type LoadingState() =
    inherit StaticWidget(NodeType.None)

    let animation = Animation.Counter(250.0)

    let animation_frames =
        [|
            Icons.CLOUD_SNOW
            Icons.CLOUD_DRIZZLE
            Icons.CLOUD_RAIN
            Icons.CLOUD_DRIZZLE
        |]

    member val Text = %"misc.loading" with get, set

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

    override this.Draw() =
        let color = (!*Palette.LIGHT, !*Palette.DARKER)
        let icon = animation_frames.[animation.Loops % animation_frames.Length]
        Text.fill_b (Style.font, icon, this.Bounds.Shrink(30.0f, 100.0f).SliceT(200.0f), color, Alignment.CENTER)

        Text.fill_b (
            Style.font,
            this.Text,
            this.Bounds.Shrink(30.0f, 100.0f).ShrinkT(175.0f).SliceT(60.0f),
            color,
            Alignment.CENTER
        )

type NewAndShiny() =
    inherit StaticWidget(NodeType.None)

    member val Icon = Icons.ALERT_CIRCLE with get, set

    override this.Draw() =
        let x, y = this.Bounds.Right, this.Bounds.Bottom // todo: alignment options
        let r = 18f
        let angle = MathF.PI / 15.0f

        let vec i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        for i = 0 to 29 do
            Render.quad_points
                (x, y)
                (x, y)
                (vec i)
                (vec (i + 1))
                Colors.red_accent

        Text.fill_b (Style.font, this.Icon, Rect.FromSize(x, y, 0.0f, 0.0f).Expand(r), Colors.text, Alignment.CENTER)