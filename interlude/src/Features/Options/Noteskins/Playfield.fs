namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Features
open Interlude.Utils
open Interlude.UI.Menu

type PlayfieldSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config

    let column_width = Setting.bounded data.ColumnWidth 10.0f 300.0f |> Setting.roundf 0

    let column_spacing =
        Setting.bounded data.ColumnSpacing 0.0f 100.0f |> Setting.roundf 0

    let fill_gaps = Setting.simple data.FillColumnGaps
    let playfield_color = Setting.simple data.PlayfieldColor
    let align_anchor = Setting.percentf (fst data.PlayfieldAlignment)
    let align_offset = Setting.percentf (snd data.PlayfieldAlignment)

    do
        this.Content(
            column ()
            |+ PageSetting("noteskins.edit.columnwidth", Slider(column_width, Step = 1f))
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnwidth"))
            |+ PageSetting("noteskins.edit.columnspacing", Slider(column_spacing, Step = 1f))
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnspacing"))

            |+ PageSetting("noteskins.edit.fillcolumngaps", Selector<_>.FromBool(fill_gaps))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.fillcolumngaps"))

            |+ PageSetting("noteskins.edit.alignmentanchor", Slider.Percent(align_anchor, Step = 0.05f))
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.alignmentanchor"))
            |+ PageSetting("noteskins.edit.alignmentoffset", Slider.Percent(align_offset, Step = 0.05f))
                .Pos(540.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.alignmentoffset"))

            |+ PageSetting("noteskins.edit.playfieldcolor", ColorPicker(playfield_color, true))
                .Pos(640.0f, PRETTYWIDTH, PRETTYHEIGHT * 2f)
                .Tooltip(Tooltip.Info("noteskins.edit.playfieldcolor"))
        )

    override this.Draw() =
        base.Draw()

        let PREVIEW_SCALE = 0.35f

        let preview_bounds =
            Rect.Box(
                this.Bounds.Right - 50.0f - this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Bottom - 50.0f - this.Bounds.Height * PREVIEW_SCALE,
                this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Height * PREVIEW_SCALE
            )

        let keys =
            match Gameplay.Chart.CACHE_DATA with
            | Some c -> c.Keys
            | None -> 4

        let frame = preview_bounds.Expand Style.PADDING
        Draw.rect (frame.SliceLeft Style.PADDING) Colors.white
        Draw.rect (frame.SliceTop Style.PADDING) Colors.white
        Draw.rect (frame.SliceRight Style.PADDING) Colors.white
        Draw.rect (frame.SliceBottom Style.PADDING) Colors.white

        let pw =
            (float32 keys * column_width.Value + float32 (keys - 1) * column_spacing.Value)
            * PREVIEW_SCALE

        let start = preview_bounds.Width * align_anchor.Value - pw * align_offset.Value
        let mutable left = start

        if fill_gaps.Value then
            Draw.rect (preview_bounds.TrimLeft(left).SliceLeft(pw)) playfield_color.Value
        else
            for i = 1 to keys do
                Draw.rect
                    (preview_bounds.TrimLeft(left).SliceLeft(column_width.Value * PREVIEW_SCALE))
                    playfield_color.Value

                left <- left + (column_width.Value + column_spacing.Value) * PREVIEW_SCALE

        Draw.rect
        <| Rect.Box(preview_bounds.Left + start, preview_bounds.CenterY - 2.5f, pw * align_offset.Value, 5f)
        <| Colors.cyan_accent.O2

        Draw.rect
        <| Rect.Box(
            preview_bounds.Left + start + pw,
            preview_bounds.CenterY - 2.5f,
            pw * (align_offset.Value - 1.0f),
            5f
        )
        <| Colors.red_accent.O2

        Draw.rect
        <| Rect.Box(
            preview_bounds.Left + preview_bounds.Width * align_anchor.Value - 2.5f,
            preview_bounds.Top,
            5f,
            preview_bounds.Height
        )
        <| Colors.green_accent.O2

    override this.Title = %"noteskins.edit.playfield.name"

    override this.OnClose() =
        Noteskins.Current.save_config
            { Noteskins.Current.config with
                ColumnWidth = column_width.Value
                ColumnSpacing = column_spacing.Value
                FillColumnGaps = fill_gaps.Value
                PlayfieldColor = playfield_color.Value
                PlayfieldAlignment = align_anchor.Value, align_offset.Value
            }
