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

    let preview = SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).TranslateY(-100.0f))

    let textures_tab = TextureGrid.Noteskin(noteskin)
    let problems_tab = ProblemList.Noteskin(noteskin)
    let general_tab =
        NavigationContainer.Column().WrapNavigation(false)
        |+ PageTextEntry(%"skin.name", name).Pos(4)
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
            .Pos(11)
        |+ PageButton(
            %"noteskin.notes",
            fun () ->
                { new NotesSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Pos(13)
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
            .Pos(15)
        |+ PageButton(
            %"noteskin.receptors",
            fun () ->
                { new AnimationSettingsPage() with
                    override this.OnClose() =
                        base.OnClose()
                        preview.Refresh()
                }
                    .Show()
        )
            .Pos(17)

    let noteskin_actions =
        NavigationContainer.Column()
        |+ PageButton(%"skins.export", fun () ->
            if not (Skins.export_skin noteskin_id) then
                Notifications.error (
                    %"notification.export_skin_failure.title",
                    %"notification.export_skin_failure.body"
                )
        )
            .Icon(Icons.UPLOAD)
            .Help(Help.Info("skins.export"))
            .Pos(0, 2, PageWidth.Full)
        |+ PageButton(
            %"skins.open_folder",
            fun () ->
                Skins.open_noteskin_folder noteskin_id |> ignore
        )
            .Icon(Icons.FOLDER)
            .Pos(2, 2, PageWidth.Full)
        |+ PageButton(%"skins.delete", fun () ->
            ConfirmPage([meta.Name] %> "noteskin.delete.confirm",
                fun () ->
                    if Skins.delete_noteskin noteskin_id then
                        Menu.Back()
            ).Show()
        )
            .TextColor(Colors.red_accent)
            .Icon(Icons.TRASH)
            .Pos(4, 2, PageWidth.Full)

    let refresh () =
        textures_tab.Refresh()
        problems_tab.Refresh()

    override this.Content() =
        refresh ()

        let tab_view_container = SwapContainer(general_tab)

        let tab_options : (Widget * string) array =
            [|
                general_tab, %"skins.general"
                textures_tab.Container, %"skins.textures"
                problems_tab.Container, %"skins.problems"
            |]

        NavigationContainer.Row()
            .With(
                NavigationContainer.Column()
                    .With(
                        TabButtons.Create(tab_options, tab_view_container)
                            .Position(page_position(0, 2, PageWidth.Normal).Translate(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceT(TabButtons.HEIGHT)),
                        tab_view_container
                            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))
                    ),

                noteskin_actions
                    .Position(Position.ShrinkL(PAGE_ITEM_WIDTH + PAGE_MARGIN_X).Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceB(PAGE_ITEM_HEIGHT * 3.0f))
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ProblemList.loader.Join()

    override this.Title = meta.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnFromNestedPage() =
        refresh ()
        preview.Refresh()
        base.OnReturnFromNestedPage()

    override this.OnClose() =
        Skins.save_skin_meta noteskin_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }