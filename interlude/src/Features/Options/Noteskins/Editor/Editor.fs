namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Gameplay

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let data = Content.NoteskinConfig

    let name = Setting.simple data.Name

    let preview = NoteskinPreview(0.35f, true)

    let grid =
        GridFlowContainer<TextureCard>(
            PRETTYWIDTH / 8f,
            8,
            WrapNavigation = false,
            Spacing = (15.0f, 15.0f),
            Position = pretty_pos(15, PAGE_BOTTOM - 15, PageWidth.Normal)
        )

    let refresh_texture_grid () =
        grid.Clear()

        for texture in Content.Noteskin.RequiredTextures do
            grid |* TextureCard(texture, (fun () -> TextureEditPage(texture).Show()))

    do
        refresh_texture_grid ()

        page_container()
        |+ PageTextEntry("noteskins.edit.noteskinname", name)
            .Pos(0)
        |+ PageButton(
            "noteskins.edit.playfield",
            fun () ->
                { new PlayfieldSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
            .Pos(3)
        |+ PageButton(
            "noteskins.edit.holdnotes",
            fun () ->
                { new HoldNoteSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
            .Pos(5)
        |+ PageButton(
            "noteskins.edit.colors",
            fun () ->
                { new ColorSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Tooltip(Tooltip.Info("noteskins.edit.colors"))
            .Pos(7)
        |+ PageButton(
            "noteskins.edit.rotations",
            fun () ->
                { new RotationSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
            .Pos(9)
        |+ PageButton(
            "noteskins.animations",
            fun () ->
                { new AnimationSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Tooltip(Tooltip.Info("noteskins.animations"))
            .Pos(11)
        |+ PageButton(
            "hud",
            fun () -> EditHUDPage().Show()
        )
            .Tooltip(Tooltip.Info("hud"))
            .Pos(13)
        |+ grid
        |>> Container
        |+ preview
        |+ Conditional(
            (fun () -> not from_hotkey),
            Callout.frame
                (Callout.Small
                    .Icon(Icons.INFO)
                    .Title(%"noteskins.edit.hotkey_hint")
                    .Hotkey("edit_noteskin"))
                (fun (w, h) -> Position.SliceTop(h).SliceRight(w).Translate(-20.0f, 20.0f))
        )
        |> this.Content

    override this.Title = data.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnTo() =
        refresh_texture_grid ()
        base.OnReturnTo()

    override this.OnClose() =
        Noteskins.save_config
            { Content.NoteskinConfig with
                Name = name.Value
            }
