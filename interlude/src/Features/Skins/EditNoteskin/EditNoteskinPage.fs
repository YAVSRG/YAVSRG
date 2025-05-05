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

    let ACTION_BUTTON_WIDTH = 450.0f

    let textures_tab = TextureGrid.Noteskin(noteskin)
    let problems_tab = ProblemList.Noteskin(noteskin)
    let general_tab =
        NavigationContainer.Column()
            .WrapNavigation(false)
            .With(
                PageTextEntry(%"skin.name", name)
                    .Pos(4),
                PageTextEntry(%"skin.author", author).Help(Help.Info("skin.author"))
                    .Pos(6),
                PageTextEntry(%"skin.editor", editor).Help(Help.Info("skin.editor"))
                    .Pos(8),
                PageButton(%"noteskin.playfield", fun () -> PlayfieldSettingsPage().Show())
                    .Pos(11, 2, PageWidth.Custom(PAGE_ITEM_WIDTH - ACTION_BUTTON_WIDTH)),
                PageButton(%"noteskin.notes", fun () -> NotesSettingsPage().Show())
                    .Pos(13, 2, PageWidth.Custom(PAGE_ITEM_WIDTH - ACTION_BUTTON_WIDTH)),
                PageButton(%"noteskin.holdnotes", fun () -> HoldNoteSettingsPage().Show())
                    .Pos(15, 2, PageWidth.Custom(PAGE_ITEM_WIDTH - ACTION_BUTTON_WIDTH)),
                PageButton(%"noteskin.receptors", fun () -> AnimationSettingsPage().Show())
                    .Pos(17, 2, PageWidth.Custom(PAGE_ITEM_WIDTH - ACTION_BUTTON_WIDTH))
            )

    let noteskin_actions =
        NavigationContainer.Column()
            .With(
                PageButton(%"skins.export", fun () ->
                    if not (Skins.export_skin noteskin_id) then
                        Notifications.error (
                            %"notification.export_skin_failure.title",
                            %"notification.export_skin_failure.body"
                        )
                )
                    .Icon(Icons.UPLOAD)
                    .Help(Help.Info("skins.export"))
                    .Pos(0, 2, PageWidth.Full),
                PageButton(%"skins.open_folder", fun () ->
                    Skins.open_noteskin_folder noteskin_id |> ignore
                )
                    .Icon(Icons.FOLDER)
                    .Pos(2, 2, PageWidth.Full),
                PageButton(%"skins.delete", fun () ->
                    ConfirmPage([meta.Name] %> "noteskin.delete.confirm",
                        fun () ->
                            if Skins.delete_noteskin noteskin_id then
                                Menu.Back()
                    )
                        .Show()
                )
                    .TextColor(Colors.red_accent)
                    .Icon(Icons.TRASH)
                    .Pos(4, 2, PageWidth.Full)
            )

    let preview = new SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).TranslateY(-100.0f))

    let refresh () =
        textures_tab.Refresh()
        problems_tab.Refresh()

    member this.SaveChanges() =
        Skins.save_skin_meta noteskin_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)
        this.DisposeOnDestroy(preview)

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
                    .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceR(ACTION_BUTTON_WIDTH).SliceB(PAGE_ITEM_HEIGHT * 3.0f))
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ProblemList.loader.Join()

    override this.Title = meta.Name

    override this.OnReturnFromNestedPage() =
        refresh ()
        preview.Refresh()
        base.OnReturnFromNestedPage()