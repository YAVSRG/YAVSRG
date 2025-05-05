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

    let preview = new SkinPreview(SkinPreview.RIGHT_HAND_SIDE(0.35f).TranslateY(-100.0f))

    let textures_tab = TextureGrid.HUD(hud)
    let problems_tab = ProblemList.HUD(hud)
    let general_tab =
        NavigationContainer.Column(WrapNavigation = false)
        |+ PageTextEntry(%"skin.name", name).Pos(0)
        |+ PageTextEntry(%"skin.author", author).Help(Help.Info("skin.author")).Pos(2)
        |+ PageTextEntry(%"skin.editor", editor).Help(Help.Info("skin.editor")).Pos(4)
        |+ PageButton( %"skins.export", fun () ->
            if not (Skins.export_skin hud_id) then
                Notifications.error (
                    %"notification.export_skin_failure.title",
                    %"notification.export_skin_failure.body"
                )
        )
            .Icon(Icons.UPLOAD)
            .Help(Help.Info("skins.export"))
            .Pos(8, 2, PageWidth.Full)
        |+ PageButton(%"skins.open_folder", fun () ->
            Skins.open_hud_folder hud_id |> ignore
        )
            .Icon(Icons.FOLDER)
            .Pos(10, 2, PageWidth.Full)
        |+ PageButton(%"skins.delete", fun () ->
            ConfirmPage([meta.Name] %> "hud.delete.confirm",
                fun () ->
                    if Skins.delete_hud hud_id then
                        Menu.Back()
            ).Show()
        )
            .TextColor(Colors.red_accent)
            .Icon(Icons.TRASH)
            .Pos(12, 2, PageWidth.Full)
    let elements_tab = ScrollContainer(ElementGrid.create(fun element -> HudElement.enabled_setting(element).Set true; ctx.Select element; Menu.Exit()))

    let refresh () =
        textures_tab.Refresh()
        problems_tab.Refresh()

    member this.SaveChanges() =
        Skins.save_skin_meta hud_id
            {
                Name = name.Value.Trim()
                Author = author.Value.Trim()
                Editor = let e = editor.Value.Trim() in if e = "" then None else Some e
            }
        ctx.CreateAll()

    override this.Content() =
        this.OnClose(this.SaveChanges)
        this.DisposeOnDestroy(preview)

        refresh ()

        let tab_view_container = SwapContainer(elements_tab)

        let tab_options : (Widget * string) array =
            [|
                elements_tab, %"hud.elements"
                general_tab, %"skins.general"
                textures_tab.Container, %"skins.textures"
                problems_tab.Container, %"skins.problems"
            |]

        NavigationContainer.Column()
            .With(
                TabButtons.Create(tab_options, tab_view_container)
                    .Position(Position.SliceT(40.0f, 60.0f).ShrinkR(PAGE_MARGIN_X).ShrinkL(PAGE_MARGIN_X + 480.0f)),
                tab_view_container
                    .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).ExpandB(80.0f))
            )
        |> Container.Create
        |+ preview.Conditional(fun () -> tab_view_container.Current <> elements_tab)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ProblemList.loader.Join()

    override this.Title = meta.Name

    override this.OnReturnFromNestedPage() =
        refresh ()
        preview.Refresh()