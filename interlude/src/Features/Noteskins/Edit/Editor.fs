namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Noteskins
open Interlude.Features.Noteskins.Edit

type EditNoteskinPage(from_hotkey: bool) =
    inherit Page()

    let noteskin_id = Skins.selected_noteskin_id.Value
    let noteskin = Content.Noteskin
    let meta = Content.NoteskinMeta

    let preview = NoteskinPreview(NoteskinPreview.RIGHT_HAND_SIDE(0.35f).Translate(0.0f, -100.0f))

    let textures_tab, refresh_texture_grid = TextureGrid.create noteskin
    let problems_tab, refresh_problems_list = Problems.create_list noteskin

    let refresh () =
        refresh_texture_grid()
        refresh_problems_list()

    override this.Content() =
        refresh ()

        let general_tab =
            NavigationContainer.Column(WrapNavigation = false)
            |+ PageButton(
                %"noteskins.edit.playfield",
                fun () ->
                    { new PlayfieldSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
                .Pos(6)
            |+ PageButton(
                %"noteskins.edit.holdnotes",
                fun () ->
                    { new HoldNoteSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
                .Pos(8)
            |+ PageButton(
                %"noteskins.edit.colors",
                fun () ->
                    { new ColorSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.colors"))
                .Pos(10)
            |+ PageButton(
                %"noteskins.edit.rotations",
                fun () ->
                    { new RotationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
                .Pos(12)
            |+ PageButton(
                %"noteskins.animations",
                fun () ->
                    { new AnimationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.animations"))
                .Pos(14)

        let tabs = SwapContainer(general_tab, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            general_tab, %"noteskins.edit.general", K false
                            textures_tab, %"noteskins.edit.textures", K false
                            problems_tab, %"noteskins.edit.problems", K false
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
                %"noteskins.edit.export",
                (fun () ->
                    if not (Skins.export_skin noteskin_id) then
                        Notifications.error (
                            %"notification.export_noteskin_failure.title",
                            %"notification.export_noteskin_failure.body"
                        )
                ),
                Icon = Icons.UPLOAD
            )
                .Tooltip(Tooltip.Info("noteskins.edit.export"))
                .Pos(0, 2, PageWidth.Full)
            |+ PageButton(
                %"noteskins.edit.open_folder",
                (fun () ->
                    Skins.open_noteskin_folder noteskin_id |> ignore
                ),
                Icon = Icons.FOLDER
            )
                .Pos(2, 2, PageWidth.Full)
            |+ PageButton(
                %"noteskins.edit.delete",
                (fun () ->
                    ConfirmPage([meta.Name] %> "noteskins.edit.delete.confirm",
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
        |+ (Callout.frame
                (Callout.Small
                    .Icon(Icons.INFO)
                    .Title(%"noteskins.edit.hotkey_hint")
                    .Hotkey("edit_noteskin"))
                (fun (w, h) -> Position.SliceTop(h).SliceRight(w).Translate(-20.0f, 20.0f))
            ).Conditional(fun () -> not from_hotkey)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Problems.problems_loader.Join()

    override this.Title = meta.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnFromNestedPage() =
        refresh ()
        base.OnReturnFromNestedPage()

    override this.OnClose() = ()
