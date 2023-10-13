namespace Interlude.Features.OptionsMenu.Noteskins

open Prelude.Charts.Tools.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Options
open Interlude.Features
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components

type NoteTextureEditPage() as this =
    inherit Page()

    let sprite = getTexture "note"
    let stitched = Setting.simple false

    do
        this.Content(
            column()
            |+ PageSetting("noteskins.edit.holdnotetrim", Selector<_>.FromBool(stitched))
                .Pos(170.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotetrim"))
        )

    override this.Draw() =
        base.Draw()
        let t = this.Bounds.Top + 50.0f
        let l = this.Bounds.Right - 50.0f - 64.0f * float32 sprite.Columns
        for x = 0 to sprite.Columns - 1 do
            for y = 0 to sprite.Rows - 1 do
                Draw.quad (Rect.Box(l + 64.0f * float32 x, t + 64.0f * float32 y, 64.0f, 64.0f) |> Quad.ofRect) (Quad.colorOf Color.White) (Sprite.gridUV (x, y) sprite)
        
    override this.Title = "WIP"
    override this.OnClose() = ()

type TextureCard(id: string, on_click: unit -> unit) as this =
    inherit Frame(NodeType.Button (fun () -> Style.click.Play(); on_click()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))

    let sprite = getTexture id

    do
        this
        |+ Text(id,
            Align = Alignment.CENTER,
            Position = Position.Margin(Style.PADDING).SliceBottom(50.0f))
        |+ Image(sprite, Position = Position.Margin(50.0f))
        |* Clickable.Focus this

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

type NoteColorPicker(color: Setting<byte>) as this =
    inherit StaticContainer(NodeType.Leaf)
    
    let sprite = getTexture "note"
    let n = byte sprite.Rows
    
    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color; Style.click.Play()
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color; Style.click.Play()
    
    do 
        this
        |* Clickable((fun () -> (if not this.Selected then this.Select()); fd ()), OnHover = fun b -> if b && not this.Focused then this.Focus())

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()
    
    override this.Draw() =
        base.Draw()
        if this.Selected then Draw.rect this.Bounds Colors.pink_accent.O2
        elif this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O2
        Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
            
        if this.Selected then
            if (!|"up").Tapped() then fd()
            elif (!|"down").Tapped() then bk()
            elif (!|"left").Tapped() then bk()
            elif (!|"right").Tapped() then fd()

type PlayfieldSettingsPage() as this =
    inherit Page()

    let data = Noteskins.Current.config
    
    let column_width = Setting.bounded data.ColumnWidth 10.0f 300.0f |> Setting.roundf 0
    let column_spacing = Setting.bounded data.ColumnSpacing 0.0f 100.0f |> Setting.roundf 0
    let playfield_color = Setting.simple data.PlayfieldColor
    let enable_column_Light = Setting.simple data.EnableColumnLight
    let align_anchor = Setting.percentf (fst data.PlayfieldAlignment)
    let align_offset = Setting.percentf (snd data.PlayfieldAlignment)

    do
        this.Content(
            column()
            |+ PageSetting("noteskins.edit.enablecolumnlight", Selector<_>.FromBool enable_column_Light)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.enablecolumnlight"))
            |+ PageSetting("noteskins.edit.columnwidth", Slider(column_width, Step = 1f))
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnwidth"))
            |+ PageSetting("noteskins.edit.columnspacing", Slider(column_spacing, Step = 1f))
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.columnspacing"))
            |+ PageSetting("noteskins.edit.alignmentanchor", Slider.Percent(align_anchor, Step = 0.05f))
                .Pos(440.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.alignmentanchor"))
            |+ PageSetting("noteskins.edit.alignmentoffset", Slider.Percent(align_offset, Step = 0.05f))
                .Pos(510.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.alignmentoffset"))
            |+ PageSetting("noteskins.edit.playfieldcolor", ColorPicker(playfield_color, true))
                .Pos(610.0f, PRETTYWIDTH, PRETTYHEIGHT * 2f)
                .Tooltip(Tooltip.Info("noteskins.edit.playfieldcolor"))
        )

    override this.Draw() =
        base.Draw()

        let PREVIEW_SCALE = 0.35f
        let preview_bounds = Rect.Box(this.Bounds.Right - 50.0f - this.Bounds.Width * PREVIEW_SCALE, this.Bounds.Bottom - 50.0f - this.Bounds.Height * PREVIEW_SCALE, this.Bounds.Width * PREVIEW_SCALE, this.Bounds.Height * PREVIEW_SCALE)
        let keys = match Gameplay.Chart.CACHE_DATA with Some c -> c.Keys | None -> 4

        let frame = preview_bounds.Expand Style.PADDING
        Draw.rect (frame.SliceLeft Style.PADDING) Colors.white
        Draw.rect (frame.SliceTop Style.PADDING) Colors.white
        Draw.rect (frame.SliceRight Style.PADDING) Colors.white
        Draw.rect (frame.SliceBottom Style.PADDING) Colors.white

        let pw = (float32 keys * column_width.Value + float32 (keys - 1) * column_spacing.Value) * PREVIEW_SCALE
        let start = preview_bounds.Width * align_anchor.Value - pw * align_offset.Value
        let mutable left = start

        for i = 1 to keys do
            Draw.rect(preview_bounds.TrimLeft(left).SliceLeft(column_width.Value * PREVIEW_SCALE)) playfield_color.Value
            left <- left + (column_width.Value + column_spacing.Value) * PREVIEW_SCALE

        Draw.rect <| Rect.Box(preview_bounds.Left + start, preview_bounds.CenterY - 2.5f, pw * align_offset.Value, 5f) <| Colors.cyan_accent.O2
        Draw.rect <| Rect.Box(preview_bounds.Left + start + pw, preview_bounds.CenterY - 2.5f, pw * (align_offset.Value - 1.0f), 5f) <| Colors.red_accent.O2
        Draw.rect <| Rect.Box(preview_bounds.Left + preview_bounds.Width * align_anchor.Value - 2.5f, preview_bounds.Top, 5f, preview_bounds.Height) <| Colors.green_accent.O2

    override this.Title = L"noteskins.edit.playfield.name"
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                EnableColumnLight = enable_column_Light.Value
                ColumnWidth = column_width.Value
                ColumnSpacing = column_spacing.Value
                PlayfieldColor = playfield_color.Value
                PlayfieldAlignment = align_anchor.Value, align_offset.Value
            }

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name
    let holdNoteTrim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
    let keycount = Setting.simple options.KeymodePreference.Value
    let mutable noteColors = data.NoteColors
        
    let g keycount i =
        let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
        Setting.make
            (fun v -> noteColors.Colors.[k].[i] <- v)
            (fun () -> noteColors.Colors.[k].[i])

    let colors, refreshColors =
        refreshRow
            (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i
                NoteColorPicker(g keycount.Value i, Position = { Position.Default with Left = 0.5f %+ (x + 120.0f * n); Right = 0.5f %+ (x + 120.0f * n + 120.0f) })
            )

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ (
                column()
                |+ PageSetting("noteskins.edit.noteskinname", TextEntry(name, "none"))
                    .Pos(200.0f)
                |+ PageSetting("noteskins.edit.holdnotetrim", Slider(holdNoteTrim))
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.holdnotetrim"))
                |+ PageButton("noteskins.edit.playfield", fun () -> PlayfieldSettingsPage().Show())
                    .Pos(370.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
                |+ PageSetting("generic.keymode",
                        Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors)) )
                    .Pos(490.0f)
                |+ PageSetting("noteskins.edit.globalcolors",
                        Selector<_>.FromBool(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                                (fun () -> noteColors.UseGlobalColors)
                            |> Setting.trigger (ignore >> refreshColors)) )
                    .Pos(560.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.globalcolors"))
                |+ PageSetting("noteskins.edit.colorstyle",
                        Selector.FromEnum(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with Style = v })
                                (fun () -> noteColors.Style)
                            |> Setting.trigger (ignore >> refreshColors)) )
                    .Pos(630.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.colorstyle"))
                |+ PageSetting("noteskins.edit.notecolors", colors)
                    .Pos(700.0f, Viewport.vwidth - 200.0f, 120.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.notecolors"))
                )
            //|+ (
            //    GridContainer<TextureCard>(200.0f, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f), Position = { Position.Margin(50.0f) with Left = 1.0f %- 470.0f })
            //    |+ TextureCard("note", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("holdhead", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("holdbody", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("holdtail", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("receptor", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("noteexplosion", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("holdexplosion", (fun () -> NoteTextureEditPage().Show()))
            //    |+ TextureCard("receptorlighting", (fun () -> NoteTextureEditPage().Show()))
            //    )
        )
        this.Add (Conditional((fun () -> not from_hotkey),
            Callout.frame (Callout.Small.Icon(Icons.info).Title(L"noteskins.edit.hotkey_hint").Hotkey("edit_noteskin")) 
                ( fun (w, h) -> Position.SliceTop(h + 40.0f + 40.0f).SliceRight(w + 40.0f).Margin(20.0f, 20.0f) )
            ))
        
    override this.Title = data.Name
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                Name = name.Value
                HoldNoteTrim = holdNoteTrim.Value
                NoteColors = noteColors
            }