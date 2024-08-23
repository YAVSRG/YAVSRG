namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.UI
open Interlude.Options

type SpacingPicker(spacing: Setting.Bounded<float32>) =
    inherit Container(NodeType.Leaf)

    let add (amount) =
        Setting.app (fun x -> x + amount) spacing
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
        |+ Text((fun () -> sprintf "%.0f" spacing.Value), Align = Alignment.CENTER, Color = K Colors.text_subheading)
        |* Clickable(
            (fun () ->
                this.Select true
                add 5.0f
            ),
            OnHover =
                (fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
                ),
            OnRightClick =
                fun () ->
                    if not this.Selected then
                        this.Select true

                    add -5.0f
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Selected then
            Draw.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O2

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Tapped() then
                add 5.0f
            elif (%%"down").Tapped() then
                add -5.0f
            elif (%%"left").Tapped() then
                add -1.0f
            elif (%%"right").Tapped() then
                add 1.0f

type PlayfieldSettingsPage() =
    inherit Page()

    let data = Content.NoteskinConfig

    let keymode: Setting<Keymode> = Setting.simple <| SelectedChart.keymode ()

    let align_anchor = Setting.percentf (fst data.PlayfieldAlignment)
    let align_offset = Setting.percentf (snd data.PlayfieldAlignment)
    let playfield_color = Setting.simple data.PlayfieldColor
    let use_stage_textures = Setting.simple data.EnableStageTextures

    let column_width = Setting.bounded data.ColumnWidth 10.0f 300.0f |> Setting.roundf 0

    let column_spacing =
        Setting.bounded data.ColumnSpacing -150.0f 300.0f |> Setting.roundf 0

    let use_advanced_column_spacing = Setting.simple data.UseAdvancedColumnSpacing
    let fill_gaps = Setting.simple data.FillColumnGaps
    let spacing = data.AdvancedColumnSpacing

    let spacing_setting keymode i =
        let k = int keymode - 3

        Setting.make (fun v -> spacing.[k].[i] <- v) (fun () -> spacing.[k].[i])
        |> Setting.roundf 0
        |> Setting.bound -150.0f 300.0f

    let NOTE_WIDTH = 120.0f

    let _spacings, refresh_spacings =
        refreshable_row
            (fun () -> int keymode.Value - 1)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i

                SpacingPicker(
                    spacing_setting keymode.Value i,
                    Position =
                        { Position.DEFAULT with
                            Left = 0.5f %+ (x + NOTE_WIDTH * n)
                            Right = 0.5f %+ (x + NOTE_WIDTH * n + NOTE_WIDTH)
                        }
                )
            )

    override this.Content() =
        page_container()
        |+ PageSetting(%"noteskin.alignmentanchor", Slider.Percent(align_anchor, Step = 0.05f))
            .Help(Help.Info("noteskin.alignmentanchor"))
            .Pos(0)
        |+ PageSetting(%"noteskin.alignmentoffset", Slider.Percent(align_offset, Step = 0.05f))
            .Help(Help.Info("noteskin.alignmentoffset"))
            .Pos(2)
        |+ PageSetting(%"noteskin.usestagetextures", Checkbox use_stage_textures)
            .Help(Help.Info("noteskin.usestagetextures"))
            .Pos(4)
        |+ PageSetting(%"noteskin.playfieldcolor", ColorPicker(playfield_color, true))
            .Pos(6, 3)
        |+ PageSetting(%"gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
            .Help(Help.Info("gameplay.hitposition"))
            .Pos(10)
        |+ PageSetting(%"noteskin.columnwidth", Slider(column_width, Step = 1f))
            .Help(Help.Info("noteskin.columnwidth"))
            .Pos(12)
        |+ PageSetting(%"noteskin.fillcolumngaps", Checkbox fill_gaps)
            .Help(Help.Info("noteskin.fillcolumngaps"))
            .Pos(14)
        |+ PageSetting(%"noteskin.useadvancedcolumnspacing", Checkbox use_advanced_column_spacing)
            .Help(Help.Info("noteskin.useadvancedcolumnspacing"))
            .Pos(16)
        |+ PageSetting(%"noteskin.columnspacing", Slider(column_spacing, Step = 1f))
            .Help(Help.Info("noteskin.columnspacing"))
            .Pos(18)
            .Conditional(use_advanced_column_spacing.Get >> not)
        |+ PageSetting(
                %"generic.keymode",
                Selector.FromEnum(keymode |> Setting.trigger (ignore >> refresh_spacings))
            )
            .Pos(18)
            .Conditional(use_advanced_column_spacing.Get)
        |+ PageSetting(%"noteskin.advancedcolumnspacing", _spacings)
            .Help(Help.Info("noteskin.advancedcolumnspacing"))
            .Pos(20, 2, PageWidth.Full)
            .Conditional(use_advanced_column_spacing.Get)
        :> Widget

    override this.Draw() =
        base.Draw()

        let PREVIEW_SCALE = 0.3f

        let preview_bounds =
            Rect.Box(
                this.Bounds.Right - 50.0f - this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Top + 50.0f,
                this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Height * PREVIEW_SCALE
            )

        let keys = int keymode.Value

        let frame = preview_bounds.Expand Style.PADDING
        Draw.rect (frame.SliceL Style.PADDING) Colors.white
        Draw.rect (frame.SliceT Style.PADDING) Colors.white
        Draw.rect (frame.SliceR Style.PADDING) Colors.white
        Draw.rect (frame.SliceB Style.PADDING) Colors.white

        let pw =
            (float32 keys * column_width.Value
             + if use_advanced_column_spacing.Value then
                   Array.sum spacing.[keys - 3]
               else
                   float32 (keys - 1) * column_spacing.Value)
            * PREVIEW_SCALE

        let start = preview_bounds.Width * align_anchor.Value - pw * align_offset.Value
        let mutable left = start

        if fill_gaps.Value then
            Draw.rect (preview_bounds.ShrinkL(left).SliceL(pw)) playfield_color.Value
        else
            for i = 1 to keys do
                Draw.rect
                    (preview_bounds.ShrinkL(left).SliceL(column_width.Value * PREVIEW_SCALE))
                    playfield_color.Value

                if i < keys then
                    let s =
                        if use_advanced_column_spacing.Value then
                            spacing.[keys - 3].[i - 1]
                        else
                            column_spacing.Value

                    left <- left + (column_width.Value + s) * PREVIEW_SCALE

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

    override this.Title = %"noteskin.playfield"

    override this.OnClose() =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                ColumnWidth = column_width.Value
                ColumnSpacing = column_spacing.Value
                FillColumnGaps = fill_gaps.Value
                PlayfieldColor = playfield_color.Value
                PlayfieldAlignment = align_anchor.Value, align_offset.Value
                UseAdvancedColumnSpacing = use_advanced_column_spacing.Value
                EnableStageTextures = use_stage_textures.Value
            }
