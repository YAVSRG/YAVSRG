namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Gameplay

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Noteskins.Current.config
        
    let name = Setting.simple data.Name

    let preview = NoteskinPreview 0.35f

    do
        this.Content(
            column()
            |+ PageTextEntry("noteskins.edit.noteskinname", name)
                .Pos(200.0f)

            |+ PageButton("noteskins.edit.playfield", fun () -> { new PlayfieldSettingsPage() with override this.OnClose() = base.OnClose(); preview.Refresh() }.Show())
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
            |+ PageButton("noteskins.edit.holdnotes", fun () -> { new HoldNoteSettingsPage() with override this.OnClose() = base.OnClose(); preview.Refresh()}.Show())
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
            |+ PageButton("noteskins.edit.colors", fun () -> { new ColorSettingsPage() with override this.OnClose() = base.OnClose(); preview.Refresh() }.Show())
                .Pos(440.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.colors"))
            |+ PageButton("noteskins.edit.rotations", fun () -> { new RotationSettingsPage() with override this.OnClose() = base.OnClose(); preview.Refresh() }.Show())
                .Pos(510.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
            |+ PageButton("noteskins.edit.animations", fun () -> { new AnimationSettingsPage() with override this.OnClose() = base.OnClose(); preview.Refresh() }.Show())
                .Pos(580.0f)
                .Tooltip(Tooltip.Info("noteskins.edit.animations"))
            |+ (
                GridFlowContainer<TextureCard>(150.0f, 4, 
                    WrapNavigation = false,
                    Spacing = (15.0f, 15.0f),
                    Position = Position.Box(0.0f, 0.0f, 100.0f, 680.0f, 645.0f, 315.0f))
                |+ TextureCard("note", (fun () -> TextureEditPage("note").Show()))
                |+ TextureCard("holdhead", (fun () -> TextureEditPage("holdhead").Show()))
                |+ TextureCard("holdbody", (fun () -> TextureEditPage("holdbody").Show()))
                |+ TextureCard("holdtail", (fun () -> TextureEditPage("holdtail").Show()))
                |+ TextureCard("receptor", (fun () -> TextureEditPage("receptor").Show()))
                |+ TextureCard("noteexplosion", (fun () -> TextureEditPage("noteexplosion").Show()))
                |+ TextureCard("holdexplosion", (fun () -> TextureEditPage("holdexplosion").Show()))
                )
            |+ preview
        )
        this.Add (Conditional((fun () -> not from_hotkey),
            Callout.frame (Callout.Small.Icon(Icons.info).Title(L"noteskins.edit.hotkey_hint").Hotkey("edit_noteskin")) 
                ( fun (w, h) -> Position.SliceTop(h + 40.0f + 40.0f).SliceRight(w + 40.0f).Margin(20.0f, 20.0f) )
            ))
        
    override this.Title = data.Name
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() =
        Noteskins.Current.changeConfig
            { Noteskins.Current.config with
                Name = name.Value
            }