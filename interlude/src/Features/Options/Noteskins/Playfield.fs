namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Features
open Interlude.Utils
open Interlude.UI.Menu
open Interlude.Options

type SpacingPicker(spacing: Setting.Bounded<float32>) =
    inherit StaticContainer(NodeType.Leaf)

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
            OnHover = (fun b -> 
                if b && not this.Focused then 
                    this.Focus true
                elif not b && this.FocusedByMouse then
                    Selection.up true
            ),
            OnRightClick = fun () ->
                if not this.Selected then this.Select true
                add -5.0f
        )
        base.Init parent

    override this.OnFocus (by_mouse: bool) =
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

type PlayfieldSettingsPage() as this =
    inherit Page()

    let data = Content.NoteskinConfig
    
    let keymode: Setting<Keymode> =
        Setting.simple <| Gameplay.Chart.keymode()

    let align_anchor = Setting.percentf (fst data.PlayfieldAlignment)
    let align_offset = Setting.percentf (snd data.PlayfieldAlignment)
    let playfield_color = Setting.simple data.PlayfieldColor
    let use_stage_textures = Setting.simple data.EnableStageTextures

    let column_width = Setting.bounded data.ColumnWidth 10.0f 300.0f |> Setting.roundf 0

    let column_spacing =
        Setting.bounded data.ColumnSpacing 0.0f 100.0f |> Setting.roundf 0

    let use_advanced_column_spacing = Setting.simple data.UseAdvancedColumnSpacing
    let fill_gaps = Setting.simple data.FillColumnGaps
    let spacing = data.AdvancedColumnSpacing

    let g keymode i =
        let k = int keymode - 3

        Setting.make (fun v -> spacing.[k].[i] <- v) (fun () -> spacing.[k].[i])
        |> Setting.roundf 0
        |> Setting.bound 0.0f 300.0f

    let NOTE_WIDTH = 120.0f

    let _spacings, refresh_spacings =
        refreshable_row
            (fun () -> int keymode.Value - 1)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i

                SpacingPicker(
                    g keymode.Value i,
                    Position =
                        { Position.Default with
                            Left = 0.5f %+ (x + NOTE_WIDTH * n)
                            Right = 0.5f %+ (x + NOTE_WIDTH * n + NOTE_WIDTH)
                        }
                )
            )

    do
        let pos = menu_pos 2.0f
        column()
        |+ PageSetting("noteskins.edit.alignmentanchor", Slider.Percent(align_anchor, Step = 0.05f))
            .Tooltip(Tooltip.Info("noteskins.edit.alignmentanchor"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.alignmentoffset", Slider.Percent(align_offset, Step = 0.05f))
            .Tooltip(Tooltip.Info("noteskins.edit.alignmentoffset"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.usestagetextures", Selector<_>.FromBool use_stage_textures)
            .Tooltip(Tooltip.Info("noteskins.edit.usestagetextures"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.playfieldcolor", ColorPicker(playfield_color, true))
            .Tooltip(Tooltip.Info("noteskins.edit.playfieldcolor"))
            .Pos(pos.Step 2.0f, PRETTYWIDTH, PRETTYHEIGHT * 1.5f)
        |+ PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
            .Tooltip(Tooltip.Info("gameplay.hitposition"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.columnwidth", Slider(column_width, Step = 1f))
            .Tooltip(Tooltip.Info("noteskins.edit.columnwidth"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.fillcolumngaps", Selector<_>.FromBool(fill_gaps))
            .Tooltip(Tooltip.Info("noteskins.edit.fillcolumngaps"))
            .Pos(pos.Step())
        |+ PageSetting("noteskins.edit.useadvancedcolumnspacing", Selector<_>.FromBool(use_advanced_column_spacing))
            .Tooltip(Tooltip.Info("noteskins.edit.useadvancedcolumnspacing"))
            .Pos(pos.Step())
        |+ Conditional(
            (fun () -> not use_advanced_column_spacing.Value),
            PageSetting("noteskins.edit.columnspacing", Slider(column_spacing, Step = 1f))
                .Tooltip(Tooltip.Info("noteskins.edit.columnspacing"))
                .Pos(pos.Y)
        )
        |+ Conditional(
            (fun () -> use_advanced_column_spacing.Value),
            PageSetting(
                "generic.keymode",
                Selector<Keymode>
                    .FromEnum(keymode |> Setting.trigger (ignore >> refresh_spacings))
            )
                .Pos(pos.Step())
        )

        |+ Conditional(
            (fun () -> use_advanced_column_spacing.Value),
            PageSetting("noteskins.edit.advancedcolumnspacing", _spacings)
                .Pos(pos.Step(), Viewport.vwidth - 200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.advancedcolumnspacing"))
        )
        |> this.Content

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
        Draw.rect (frame.SliceLeft Style.PADDING) Colors.white
        Draw.rect (frame.SliceTop Style.PADDING) Colors.white
        Draw.rect (frame.SliceRight Style.PADDING) Colors.white
        Draw.rect (frame.SliceBottom Style.PADDING) Colors.white

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
            Draw.rect (preview_bounds.TrimLeft(left).SliceLeft(pw)) playfield_color.Value
        else
            for i = 1 to keys do
                Draw.rect
                    (preview_bounds.TrimLeft(left).SliceLeft(column_width.Value * PREVIEW_SCALE))
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

    override this.Title = %"noteskins.edit.playfield.name"

    override this.OnClose() =
        Noteskins.save_config
            { Content.NoteskinConfig with
                ColumnWidth = column_width.Value
                ColumnSpacing = column_spacing.Value
                FillColumnGaps = fill_gaps.Value
                PlayfieldColor = playfield_color.Value
                PlayfieldAlignment = align_anchor.Value, align_offset.Value
                UseAdvancedColumnSpacing = use_advanced_column_spacing.Value
                EnableStageTextures = use_stage_textures.Value
            }
