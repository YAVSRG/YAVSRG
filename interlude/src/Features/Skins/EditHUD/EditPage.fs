namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.Gameplay

type EditHUDPage() =
    inherit Page()

    let hud_id = Skins.selected_hud_id.Value
    let hud = Skins.current_hud
    let meta = Content.HUDMeta

    let name = Setting.simple meta.Name
    let author = Setting.simple meta.Author
    let editor = Setting.simple (meta.Editor |> Option.defaultValue "")

    let preview = SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).Translate(0.0f, -100.0f))

    let textures_tab, refresh_texture_grid = TextureGrid.create_hud hud
    let problems_tab, refresh_problems_list = Problems.create_hud hud
    let general_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageTextEntry(%"skin.name", name).Pos(4)
        |+ PageTextEntry(%"skin.author", author).Help(Help.Info("skin.author")).Pos(6)
        |+ PageTextEntry(%"skin.editor", editor).Help(Help.Info("skin.editor")).Pos(8)

    let refresh () =
        refresh_texture_grid()
        refresh_problems_list()

    let meta_editor =
        let tabs = SwapContainer(general_tab, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
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
            NavigationContainer.Column(Position = Position.ShrinkL(PRETTYWIDTH + PRETTY_MARGIN_X).Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceB(PRETTYHEIGHT * 3.0f))
            |+ PageButton(
                %"skins.export",
                (fun () ->
                    if not (Skins.export_skin hud_id) then
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
                    Skins.open_hud_folder hud_id |> ignore
                ),
                Icon = Icons.FOLDER
            )
                .Pos(2, 2, PageWidth.Full)
            |+ PageButton(
                %"skins.delete",
                (fun () ->
                    ConfirmPage([meta.Name] %> "hud.delete.confirm",
                        fun () ->
                            if Skins.delete_hud hud_id then
                                Menu.Back()
                    ).Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(4, 2, PageWidth.Full)
        )
        |>> Container
        |+ preview

    let layout_editor =
        let mutable output : Widget = Dummy()
        SelectedChart.if_loaded(fun info -> output <- LayoutEditor(info, { Left = 0.1f %+ 0.0f; Top = 0.1f %+ 0.0f; Right = 0.9f %- 0.0f; Bottom = 0.9f %- 0.0f }))
        output

    let supertabs = SwapContainer(layout_editor)

    let editing_meta =
        Setting.simple false
        |> Setting.trigger (function true -> supertabs.Current <- meta_editor | false -> supertabs.Current <- layout_editor)

    let header = EditHUDHeader(editing_meta)

    override this.Content() =
        refresh ()
        supertabs

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Problems.problems_loader.Join()

    override this.Title = meta.Name
    override this.OnDestroy() = preview.Destroy()

    override this.Init(parent) =
        base.Init parent
        header.Focus false

    override this.Header() =
        header
        |> OverlayContainer
        :> Widget

    override this.OnEnterNestedPage() = header.Hide()

    override this.OnReturnFromNestedPage() =
        refresh ()
        preview.Refresh()
        header.Show()

    override this.OnClose() =
        match layout_editor with
        | :? LayoutEditor as l -> l.Destroy()
        | _ -> ()
        header.Hide()
        Skins.save_skin_meta hud_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }