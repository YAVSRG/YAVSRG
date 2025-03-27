namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude

type ColorPickerPage(title: string, color: Setting<Color>, allow_alpha: bool, on_close: unit -> unit) =
    inherit Page()

    let mutable hex = color.Value.ToHex()

    let mutable r = float32 color.Value.R
    let mutable g = float32 color.Value.G
    let mutable b = float32 color.Value.B

    let mutable a = float32 color.Value.A

    let h, s, v = color.Value.ToHsv()
    let mutable h = h
    let mutable s = s
    let mutable v = v

    let update_rgb() =
        color.Value <- Color.FromArgb(int a, int r, int g, int b)
        let _h, _s, _v = color.Value.ToHsv()
        h <- _h
        s <- _s
        v <- _v
        hex <- color.Value.ToHex()

    let update_hsv() =
        color.Value <- Color.FromHsv(h, s, v).O4a(int a)
        r <- float32 color.Value.R
        g <- float32 color.Value.G
        b <- float32 color.Value.B
        hex <- color.Value.ToHex()

    let hex_color_setting =
        color
        |> Setting.trigger (fun color ->
            if allow_alpha then a <- float32 color.A
            r <- float32 color.R
            g <- float32 color.G
            b <- float32 color.B
            update_rgb()
        )

    let SEGMENTS = 6
    let SEGMENT_SIZE = 1.0f / float32 SEGMENTS

    let draw_bar (color_func: float32 -> Color) (bounds: Rect, percentage: float32) =
        for i = 0 to SEGMENTS - 1 do
            Render.rect_c
                (bounds.SlicePercentL(float32 i * SEGMENT_SIZE, SEGMENT_SIZE))
                (Quad.gradient_left_to_right
                    (color_func(float32 i * SEGMENT_SIZE))
                    (color_func(float32 (i + 1) * SEGMENT_SIZE))
                )

        let cursor = bounds.ShrinkPercentL(percentage).SliceL(0.0f)
        Render.rect (cursor.Expand(5f, 5f)) (if v > 0.5f then Colors.black else Colors.white)
        Render.rect (cursor.Expand(2.5f, 2.5f)) (color_func percentage)

    let red = Setting.make (fun x -> r <- x; update_rgb()) (fun () -> r) |> Setting.bound (0.0f, 255.0f)
    let green = Setting.make (fun x -> g <- x; update_rgb()) (fun () -> g) |> Setting.bound (0.0f, 255.0f)
    let blue = Setting.make (fun x -> b <- x; update_rgb()) (fun () -> b) |> Setting.bound (0.0f, 255.0f)

    let alpha = Setting.make (fun x -> a <- x; update_rgb()) (fun () -> a) |> Setting.bound (0.0f, 255.0f)

    let hue = Setting.make (fun x -> h <- x / 360.0f; update_hsv()) (fun () -> h * 360.0f) |> Setting.bound (0.0f, 360.0f)
    let saturation = Setting.make (fun x -> s <- x; update_hsv()) (fun () -> s) |> Setting.bound (0.0f, 1.0f)
    let value = Setting.make (fun x -> v <- x; update_hsv()) (fun () -> v) |> Setting.bound (0.0f, 1.0f)

    let red_slider =
        PageSetting("Red",
            { new Slider(red, Format = sprintf "%.0f", Step = 5.0f) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromArgb(int (x * 255.0f), int g, int b)) (bounds, percentage)
            }
        )
    let green_slider =
        PageSetting("Green",
            { new Slider(green, Format = sprintf "%.0f", Step = 5.0f) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromArgb(int r, int (x * 255.0f), int b)) (bounds, percentage)
            }
        )
    let blue_slider =
        PageSetting("Blue",
            { new Slider(blue, Format = sprintf "%.0f", Step = 5.0f) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromArgb(int r, int g, int (x * 255.0f))) (bounds, percentage)
            }
        )

    let alpha_slider =
        PageSetting("Alpha",
            { new Slider(alpha, Format = sprintf "%.0f", Step = 5.0f) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromArgb(int (x * 255.0f), int r, int g, int b)) (bounds, percentage)
            }
        )

    let hue_slider =
        PageSetting("Hue",
            { new Slider(hue, Format = sprintf "%.0f'", Step = 5.0f) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromHsv(x, s, v)) (bounds, percentage)
            }
        )
    let saturation_slider =
        PageSetting("Saturation",
            { new Slider(saturation, Format = fun v -> sprintf "%.0f%%" (v * 100.0f)) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromHsv(h, x, v)) (bounds, percentage)
            }
        )
    let value_slider =
        PageSetting("Value",
            { new Slider(value, Format = fun v -> sprintf "%.0f%%" (v * 100.0f)) with
                override this.DrawBar(bounds, percentage) =
                    draw_bar (fun x -> Color.FromHsv(h, s, x)) (bounds, percentage)
            }
        )

    override this.Content() =
        page_container()
        |+ PageSetting("Hex", NumberEntry.Create(hex_color_setting)).Pos(0)
        |+ red_slider.Pos(3)
        |+ green_slider.Pos(5)
        |+ blue_slider.Pos(7)
        |> fun c ->
            if allow_alpha then
                c
                |+ alpha_slider.Pos(10)
                |+ hue_slider.Pos(13)
                |+ saturation_slider.Pos(15)
                |+ value_slider.Pos(17)
            else
                c
                |+ hue_slider.Pos(10)
                |+ saturation_slider.Pos(12)
                |+ value_slider.Pos(14)
        :> Widget

    override this.Title = title
    override this.OnClose() = on_close()

type ColorPicker(label: string, color: Setting<Color>, allow_alpha: bool) as this =
    inherit Container(NodeType.Button(fun _ -> this.Edit()))

    let mutable hex = color.Value.ToHex()

    override this.Init (parent: Widget) =
        this |* MouseListener().Button(this)
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let preview = this.Bounds.SliceY(PAGE_ITEM_HEIGHT * 0.6f).SliceL(50.0f).Shrink(5.0f)
        Render.rect preview color.Value
        Text.fill_b(Style.font, hex, this.Bounds.ShrinkL(60.0f), (color.Value.O4, Colors.black), Alignment.LEFT)

    member this.Edit() =
        Style.click.Play()
        ColorPickerPage(label, color, allow_alpha, fun () -> hex <- color.Value.ToHex()).Show()