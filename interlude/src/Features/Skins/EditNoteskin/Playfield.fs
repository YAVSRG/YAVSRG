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

    let add (amount: float32) =
        Setting.app (fun x -> x + amount) spacing

    override this.Init(parent: Widget) =
        this
        |+ Text((fun () -> sprintf "%.0f" spacing.Value))
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
        |* MouseListener()
            .SelectOnClick(this, fun () ->
                add 5.0f
                Style.click.Play()
            )
            .FocusOnHover(this)
            .OnRightClick(fun () ->
                this.Select true
                add -5.0f
                Style.click.Play()
            )
            .Floating()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Selected then
            Render.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"up").Pressed() then
                add 5.0f
                Style.click.Play()
            elif (%%"down").Pressed() then
                add -5.0f
                Style.click.Play()
            elif (%%"left").Pressed() then
                add -1.0f
                Style.click.Play()
            elif (%%"right").Pressed() then
                add 1.0f
                Style.click.Play()

        if Mouse.hover this.Bounds then
            add (Mouse.scroll())

type PlayfieldSettingsPage() =
    inherit Page()

    let data = Content.NoteskinConfig

    let keymode: Setting<Keymode> = Setting.simple <| SelectedChart.keymode ()

    let align_anchor = Setting.percentf (fst data.PlayfieldAlignment)
    let align_offset = Setting.percentf (snd data.PlayfieldAlignment)
    let playfield_color = Setting.simple data.PlayfieldColor
    let use_stage_textures = Setting.simple data.EnableStageTextures

    let column_width =
        data.ColumnWidth
        |> Setting.bounded (10.0f, 300.0f)
        |> Setting.roundf 0

    let use_specific_column_widths = Setting.simple data.UseKeymodeSpecificColumnWidth
    let column_widths = data.KeymodeSpecificColumnWidth

    let column_spacing =
        data.ColumnSpacing
        |> Setting.bounded (-150.0f, 300.0f)
        |> Setting.roundf 0

    let use_advanced_column_spacing = Setting.simple data.UseAdvancedColumnSpacing
    let spacing = data.AdvancedColumnSpacing
    let fill_gaps = Setting.simple data.FillColumnGaps

    let spacing_setting keymode i =
        let k = int keymode - 3

        Setting.make (fun v -> spacing.[k].[i] <- v) (fun () -> spacing.[k].[i])
        |> Setting.bound (-150.0f, 300.0f)
        |> Setting.roundf 0

    let width_setting =
        Setting.make
            (fun v -> if use_specific_column_widths.Value then column_widths.[int keymode.Value - 3] <- v else column_width.Set v)
            (fun () -> if use_specific_column_widths.Value then column_widths.[int keymode.Value - 3] else column_width.Value)
        |> Setting.bound (10.0f, 300.0f)
        |> Setting.roundf 0

    let PICKER_WIDTH = 120.0f

    let _spacings, refresh_spacings =
        refreshable_row
            (fun () -> int keymode.Value - 1)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i

                SpacingPicker(spacing_setting keymode.Value i)
                    .Position(
                        { Position.DEFAULT with
                            Left = 0.5f %+ (x + PICKER_WIDTH * n)
                            Right = 0.5f %+ (x + PICKER_WIDTH * n + PICKER_WIDTH)
                        }
                    )
            )

    member this.SaveChanges() =
        Skins.save_noteskin_config
            { Content.NoteskinConfig with
                UseKeymodeSpecificColumnWidth = use_specific_column_widths.Value
                ColumnWidth = column_width.Value
                KeymodeSpecificColumnWidth = column_widths
                ColumnSpacing = column_spacing.Value
                FillColumnGaps = fill_gaps.Value
                PlayfieldColor = playfield_color.Value
                PlayfieldAlignment = align_anchor.Value, align_offset.Value
                UseAdvancedColumnSpacing = use_advanced_column_spacing.Value
                EnableStageTextures = use_stage_textures.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"noteskin.alignmentanchor", Slider.Percent(align_anchor, Step = 0.05f))
                    .Help(Help.Info("noteskin.alignmentanchor"))
                    .Pos(0),
                PageSetting(%"noteskin.alignmentoffset", Slider.Percent(align_offset, Step = 0.05f))
                    .Help(Help.Info("noteskin.alignmentoffset"))
                    .Pos(2),
                PageSetting(%"noteskin.usestagetextures", Checkbox use_stage_textures)
                    .Help(Help.Info("noteskin.usestagetextures"))
                    .Pos(4),
                PageSetting(%"noteskin.playfieldcolor", ColorPicker(%"noteskin.playfieldcolor", playfield_color, true))
                    .Pos(6),

                PageSetting(
                        %"generic.keymode",
                        SelectDropdown.FromEnum(keymode |> Setting.trigger (ignore >> refresh_spacings))
                    )
                    .Pos(10)
                    .Conditional(fun () -> use_specific_column_widths.Value || use_advanced_column_spacing.Value),

                PageSetting(%"noteskin.keymode_specific_column_widths", Checkbox use_specific_column_widths)
                    .Pos(14),
                PageSetting(%"noteskin.columnwidth", Slider(width_setting, Step = 1f))
                    .Help(Help.Info("noteskin.columnwidth"))
                    .Pos(16),

                PageSetting(%"noteskin.fillcolumngaps", Checkbox fill_gaps)
                    .Help(Help.Info("noteskin.fillcolumngaps"))
                    .Pos(18),
                PageSetting(%"noteskin.useadvancedcolumnspacing", Checkbox use_advanced_column_spacing)
                    .Help(Help.Info("noteskin.useadvancedcolumnspacing"))
                    .Pos(20),
                PageSetting(%"noteskin.columnspacing", Slider(column_spacing, Step = 1f))
                    .Help(Help.Info("noteskin.columnspacing"))
                    .Pos(22)
                    .Conditional(use_advanced_column_spacing.Get >> not),
                PageSetting(%"noteskin.advancedcolumnspacing", _spacings)
                    .Help(Help.Info("noteskin.advancedcolumnspacing"))
                    .Pos(22, 2, PageWidth.Full)
                    .Conditional(use_advanced_column_spacing.Get)
            )

    override this.Draw() =
        base.Draw()

        let PREVIEW_SCALE = 0.3f

        let preview_bounds =
            Rect.FromSize(
                this.Bounds.Right - 50.0f - this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Top + 50.0f,
                this.Bounds.Width * PREVIEW_SCALE,
                this.Bounds.Height * PREVIEW_SCALE
            )

        let keys = int keymode.Value

        let frame = preview_bounds.Expand Style.PADDING
        Render.rect (frame.SliceL Style.PADDING) Colors.white
        Render.rect (frame.SliceT Style.PADDING) Colors.white
        Render.rect (frame.SliceR Style.PADDING) Colors.white
        Render.rect (frame.SliceB Style.PADDING) Colors.white

        let pw =
            (float32 keys * width_setting.Value
             + if use_advanced_column_spacing.Value then
                   Array.sum spacing.[keys - 3]
               else
                   float32 (keys - 1) * column_spacing.Value)
            * PREVIEW_SCALE

        let start = preview_bounds.Width * align_anchor.Value - pw * align_offset.Value
        let mutable left = start

        if fill_gaps.Value then
            Render.rect (preview_bounds.ShrinkL(left).SliceL(pw)) playfield_color.Value
        else
            for i = 1 to keys do
                Render.rect
                    (preview_bounds.ShrinkL(left).SliceL(width_setting.Value * PREVIEW_SCALE))
                    playfield_color.Value

                if i < keys then
                    let s =
                        if use_advanced_column_spacing.Value then
                            spacing.[keys - 3].[i - 1]
                        else
                            column_spacing.Value

                    left <- left + (width_setting.Value + s) * PREVIEW_SCALE

        Render.rect_size
            (preview_bounds.Left + start)
            (preview_bounds.CenterY - 2.5f)
            (pw * align_offset.Value)
            5f
            Colors.cyan_accent.O2

        Render.rect_size
            (preview_bounds.Left + start + pw)
            (preview_bounds.CenterY - 2.5f)
            (pw * (align_offset.Value - 1.0f))
            5f
            Colors.red_accent.O2

        Render.rect_size
            (preview_bounds.Left + preview_bounds.Width * align_anchor.Value - 2.5f)
            preview_bounds.Top
            5f
            preview_bounds.Height
            Colors.green_accent.O2

    override this.Title = %"noteskin.playfield"