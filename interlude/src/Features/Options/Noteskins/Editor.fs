namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

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

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ (
                column()
                |+ PageTextEntry("noteskins.edit.noteskinname", name)
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
                |+ PageButton("noteskins.edit.rotations", fun () -> RotationSettingsPage().Show())
                    .Pos(510.0f)
                    .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
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