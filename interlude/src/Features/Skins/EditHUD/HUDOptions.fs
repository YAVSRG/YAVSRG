namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Skins

type EditHUDPage(ctx: PositionerContext) =
    inherit Page()

    let hud_id = Skins.selected_hud_id.Value
    let hud = Skins.current_hud
    let meta = Content.HUDMeta

    let name = Setting.simple meta.Name
    let author = Setting.simple meta.Author
    let editor = Setting.simple (meta.Editor |> Option.defaultValue "")

    let preview = SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).TranslateY(-100.0f))

    let textures_tab = TextureGrid.HUD(hud)
    let problems_tab = ProblemList.HUD(hud)
    let general_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageTextEntry(%"skin.name", name).Pos(0)
        |+ PageTextEntry(%"skin.author", author).Help(Help.Info("skin.author")).Pos(2)
        |+ PageTextEntry(%"skin.editor", editor).Help(Help.Info("skin.editor")).Pos(4)
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
            .Pos(8, 2, PageWidth.Full)
        |+ PageButton(
            %"skins.open_folder",
            (fun () ->
                Skins.open_hud_folder hud_id |> ignore
            ),
            Icon = Icons.FOLDER
        )
            .Pos(10, 2, PageWidth.Full)
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
            .Pos(12, 2, PageWidth.Full)
    let elements_tab = ScrollContainer(ElementGrid.create(fun element -> HudElement.enabled_setting(element).Set true; ctx.Select element; Menu.Exit()))

    let refresh () =
        textures_tab.Refresh()
        problems_tab.Refresh()

    override this.Content() =
        refresh ()
        let tabs =
            SwapContainer(elements_tab)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).ExpandB(80.0f))
        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            elements_tab, %"hud.elements", K false
                            general_tab, %"skins.general", K false
                            textures_tab.Container, %"skins.textures", K false
                            problems_tab.Container, %"skins.problems", K false
                        |]
                    Height = 60.0f
                }

        tab_buttons.Position <- Position.SliceT(40.0f, 60.0f).ShrinkR(PAGE_MARGIN_X).ShrinkL(PAGE_MARGIN_X + 480.0f)

        NavigationContainer.Column()
            .With(tab_buttons, tabs)
        |> Container.Create
        |+ preview.Conditional(fun () -> tabs.Current <> elements_tab)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ProblemList.loader.Join()

    override this.Title = meta.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnFromNestedPage() =
        refresh ()
        preview.Refresh()

    override this.OnClose() =
        Skins.save_skin_meta hud_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }
        ctx.CreateAll()