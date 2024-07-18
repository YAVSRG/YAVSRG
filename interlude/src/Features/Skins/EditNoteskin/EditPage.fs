namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.Skins.EditNoteskin

type EditNoteskinPage() =
    inherit Page()

    let noteskin_id = Skins.selected_noteskin_id.Value
    let noteskin = Content.Noteskin
    let meta = Content.NoteskinMeta

    let name = Setting.simple meta.Name
    let author = Setting.simple meta.Author
    let editor = Setting.simple (meta.Editor |> Option.defaultValue "")

    let preview = SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).Translate(0.0f, -100.0f))

    let textures_tab, refresh_texture_grid = TextureGrid.create_noteskin noteskin
    let problems_tab, refresh_problems_list = Problems.create_noteskin noteskin

    let refresh () =
        refresh_texture_grid()
        refresh_problems_list()

    override this.Content() =
        refresh ()

        let general_tab =
            NavigationContainer.Column(WrapNavigation = false)
            |+ PageTextEntry(%"skin.name", name).Help(Help.Info("skin.name")).Pos(4)
            |+ PageTextEntry(%"skin.author", author).Help(Help.Info("skin.author")).Pos(6)
            |+ PageTextEntry(%"skin.editor", editor).Help(Help.Info("skin.editor")).Pos(8)
            |+ PageButton(
                %"noteskin.playfield",
                fun () ->
                    { new PlayfieldSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Help(Help.Info("noteskin.playfield"))
                .Pos(11)
            |+ PageButton(
                %"noteskin.holdnotes",
                fun () ->
                    { new HoldNoteSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Help(Help.Info("noteskin.holdnotes"))
                .Pos(13)
            |+ PageButton(
                %"noteskin.colors",
                fun () ->
                    { new ColorSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Help(Help.Info("noteskin.colors"))
                .Pos(15)
            |+ PageButton(
                %"noteskin.rotations",
                fun () ->
                    { new RotationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Help(Help.Info("noteskin.rotations"))
                .Pos(17)
            |+ PageButton(
                %"noteskin.animations",
                fun () ->
                    { new AnimationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Help(Help.Info("noteskin.animations"))
                .Pos(19)

        let tabs = SwapContainer(general_tab, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            general_tab, %"skins.general", K false
                            textures_tab, %"skins.textures", K false
                            problems_tab, %"skins.problems", K false
                        |]
                    Height = 50.0f
                }

        tab_buttons.Position <- pretty_pos(0, 2, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)

        NavigationContainer.Row()
        |+ (
            NavigationContainer.Column()
            |+ tab_buttons
            |+ tabs
        )
        |+ (
            NavigationContainer.Column(Position = Position.TrimLeft(PRETTYWIDTH + PRETTY_MARGIN_X).Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceBottom(PRETTYHEIGHT * 3.0f))
            |+ PageButton(
                %"skins.export",
                (fun () ->
                    if not (Skins.export_skin noteskin_id) then
                        Notifications.error (
                            %"notification.export_skin_failure.title",
                            %"notification.export_skin_failure.body"
                        )
                ),
                Icon = Icons.UPLOAD
            )
                .Help(Help.Info("skins.export"))
                .Pos(0, 2, PageWidth.Full)
            |+ PageButton(
                %"skins.open_folder",
                (fun () ->
                    Skins.open_noteskin_folder noteskin_id |> ignore
                ),
                Icon = Icons.FOLDER
            )
                .Pos(2, 2, PageWidth.Full)
            |+ PageButton(
                %"skins.delete",
                (fun () ->
                    ConfirmPage([meta.Name] %> "noteskin.delete.confirm",
                        fun () ->
                            if Skins.delete_noteskin noteskin_id then
                                Menu.Back()
                    ).Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(4, 2, PageWidth.Full)
        )
        |>> Container
        |+ preview
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Problems.problems_loader.Join()

    override this.Title = meta.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnFromNestedPage() =
        refresh ()
        base.OnReturnFromNestedPage()

    override this.OnClose() =
        Skins.save_skin_meta noteskin_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }
