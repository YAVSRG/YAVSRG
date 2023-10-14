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

// ----

type NoteColorPicker(color: Setting<byte>, style: ColorScheme, index: int) as this =
    inherit StaticContainer(NodeType.Leaf)
    
    let sprite = getTexture "note"
    let n = byte sprite.Rows
    
    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color; Style.click.Play()
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color; Style.click.Play()
    
    do 
        this
        |+ Tooltip(Callout.Normal
            .Title(sprintf "%s: %O" (L"noteskins.edit.notecolors.name") style)
            .Body(L (sprintf "noteskins.edit.notecolors.%s.%i" (style.ToString().ToLower()) index)))
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

type ColorSettingsPage() as this =
    inherit Page()
    
    let data = Noteskins.Current.config
    let keycount = Setting.simple options.KeymodePreference.Value
    let mutable noteColors = data.NoteColors
    
    let g keycount i =
        let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
        Setting.make
            (fun v -> noteColors.Colors.[k].[i] <- v)
            (fun () -> noteColors.Colors.[k].[i])

    let NOTE_WIDTH = 120.0f

    let colors, refreshColors =
        refreshRow
            (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style)
            (fun i k ->
                let x = -60.0f * float32 k
                let n = float32 i
                NoteColorPicker(g keycount.Value i, noteColors.Style, i, 
                    Position = { Position.Default with Left = 0.5f %+ (x + NOTE_WIDTH * n); Right = 0.5f %+ (x + NOTE_WIDTH * n + NOTE_WIDTH) })
            )

    do
        this.Content(
            column()
            |+ PageSetting("noteskins.edit.globalcolors",
                    Selector<_>.FromBool(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                            (fun () -> noteColors.UseGlobalColors)
                        |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.globalcolors"))
            |+ PageSetting("generic.keymode",
                    Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(270.0f)
            |+ PageSetting("noteskins.edit.colorstyle",
                    Selector.FromEnum(
                        Setting.make
                            (fun v -> noteColors <- { noteColors with Style = v })
                            (fun () -> noteColors.Style)
                        |> Setting.trigger (ignore >> refreshColors)) )
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.colorstyle"))
            |+ PageSetting("noteskins.edit.notecolors", colors)
                .Pos(470.0f, Viewport.vwidth - 200.0f, NOTE_WIDTH)
            )
        
    override this.Title = L"noteskins.edit.colors.name"
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                NoteColors = noteColors
            }

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ (
                column()
                |+ PageSetting("noteskins.edit.noteskinname", TextEntry(name, "none"))
                    .Pos(200.0f)

                |+ PageButton("noteskins.edit.playfield", fun () -> PlayfieldSettingsPage().Show())
                    .Pos(300.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
                |+ PageButton("noteskins.edit.holdnotes", fun () -> HoldNoteSettingsPage().Show())
                    .Pos(370.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
                |+ PageButton("noteskins.edit.colors", fun () -> ColorSettingsPage().Show())
                    .Pos(440.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.colors"))
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
            }