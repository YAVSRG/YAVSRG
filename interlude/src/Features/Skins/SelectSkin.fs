namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins
open Interlude.Content
open Interlude.Features.Online
open Interlude.Options
open Interlude.UI
open Interlude.Features.Import
open Interlude.Features.Gameplay
open Interlude.Features.Skins.EditNoteskin
open Interlude.Features.Skins.EditHUD
open Interlude.Features.Skins.Browser

type private NoteskinButton(id: string, meta: SkinMetadata, on_switch: unit -> unit, on_edit: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                if Skins.selected_noteskin_id.Value <> id then
                    options.Noteskin.Set id
                    on_switch ()
                else
                    on_edit()
            )
        )

    let credit =
        match meta.Editor with
        | Some e -> [meta.Author; e] %> "skins.credit.edited"
        | None -> [meta.Author] %> "skins.credit"

    member this.IsCurrent = Skins.selected_noteskin_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(meta.Name)
            .Color(fun () ->
                if this.Focused then Colors.text_yellow_2
                elif this.IsCurrent then Colors.text_pink
                else Colors.text
            )
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(70.0f).ShrinkX(Style.PADDING).SliceT(45.0f))
        |+ Text(
            (fun () ->
                if this.Focused then
                    if this.IsCurrent then
                        if this.FocusedByMouse then %"skins.edit_hint_mouse" else [(%%"select").ToString()] %> "skins.edit_hint_keyboard"
                    else
                        if this.FocusedByMouse then %"skins.use_hint_mouse" else [(%%"select").ToString()] %> "skins.use_hint_keyboard"
                else credit
            ))
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(70.0f).Shrink(Style.PADDING).SliceB(30.0f))
        |* MouseListener().Button(this)

        match Skins.get_icon id with
        | Some sprite ->
            this.Add(
                Image(sprite)
                    .Position(Position.SliceL(70.0f).Shrink(Style.PADDING))
            )
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Render.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type private HUDButton(id: string, meta: SkinMetadata, on_switch: unit -> unit, on_edit: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                if Skins.selected_hud_id.Value <> id then
                    options.SelectedHUD.Set id
                    on_switch ()
                else
                    on_edit ()
            )
        )

    let credit =
        match meta.Editor with
        | Some e -> [meta.Author; e] %> "skins.credit.edited"
        | None -> [meta.Author] %> "skins.credit"

    member this.IsCurrent = Skins.selected_hud_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(meta.Name)
            .Color(fun () ->
                if this.Focused then Colors.text_yellow_2
                elif this.IsCurrent then Colors.text_green
                else Colors.text
            )
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(70.0f).ShrinkX(Style.PADDING).SliceT(45.0f))
        |+ Text(
            (fun () ->
                if this.Focused then
                    if this.IsCurrent then
                        if this.FocusedByMouse then %"skins.edit_hint_mouse" else [(%%"select").ToString()] %> "skins.edit_hint_keyboard"
                    else
                        if this.FocusedByMouse then %"skins.use_hint_mouse" else [(%%"select").ToString()] %> "skins.use_hint_keyboard"
                else credit
            ))
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(70.0f).Shrink(Style.PADDING).SliceB(30.0f))
        |* MouseListener().Button(this)

        match Skins.get_icon id with
        | Some sprite ->
            this.Add(
                Image(sprite)
                    .Position(Position.SliceL(70.0f).Shrink(Style.PADDING))
            )
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Render.rect this.Bounds Colors.green_accent.O1
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

module SkinActions =

    let edit_hud (on_exit: unit -> unit) : unit =
        if
            SelectedChart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> EditHudScreen.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, on_exit))
                ScreenType.EditHud
                Transitions.Default
        then
            Menu.Exit()

    let edit_or_extract_noteskin () : unit =
        let noteskin = Content.Noteskin

        if noteskin.IsEmbedded then
            ConfirmPage(
                %"skins.confirm_extract_default",
                (fun () ->
                    if
                        Skins.create_user_noteskin_from_default (
                            if Network.credentials.Username <> "" then
                                Some Network.credentials.Username
                            else
                                None
                        )
                    then
                        GameThread.defer (fun () -> EditNoteskinPage().Show())
                    else
                        Notifications.error(%"skins.extract_default_failed.title", %"skins.extract_default_failed.body")
                        Logging.Error "Error extracting noteskin to be editable"
                )
            )
                .Show()
        else
            EditNoteskinPage().Show()

type SelectSkinsPage() =
    inherit Page()

    let preview = new SkinPreview(SkinPreview.LEFT_HAND_SIDE 0.35f)

    let noteskin_grid =
        GridFlowContainer<NoteskinButton>(70.0f, 1)
            .WrapNavigation(false)
            .Spacing(20.0f)

    let hud_grid =
        GridFlowContainer<HUDButton>(70.0f, 1)
            .WrapNavigation(false)
            .Spacing(20.0f)

    let noteskin_tab =
        ScrollContainer(noteskin_grid)
            .Position(Position.SlicePercentL(0.5f).ShrinkT(110.0f).ShrinkR(Style.PADDING))
    let hud_tab =
        ScrollContainer(hud_grid)
            .Position(Position.SlicePercentR(0.5f).ShrinkT(110.0f).ShrinkL(Style.PADDING))

    let refresh () =
        preview.Refresh()

        noteskin_grid.Clear()
        for id, _, meta in Skins.list_noteskins () do
            let nb = NoteskinButton(id, meta, preview.Refresh, SkinActions.edit_or_extract_noteskin)
            if nb.IsCurrent then GameThread.defer (fun () -> noteskin_tab.ScrollTo(nb))
            noteskin_grid |* nb

        hud_grid.Clear()
        for id, _, meta in Skins.list_huds () do
            let hb = HUDButton(id, meta, preview.Refresh, fun () -> SkinActions.edit_hud (fun () -> SelectSkinsPage().Show()))
            if hb.IsCurrent then GameThread.defer (fun () -> hud_tab.ScrollTo(hb))
            hud_grid |* hb

    override this.Content() =
        this.DisposeOnDestroy(preview)
        refresh ()

        let left_info =
            NavigationContainer.Column()
                .Position(
                    Position
                        .SlicePercentL(0.35f)
                        .ShrinkR(Style.PADDING * 2.0f)
                        .ShrinkL(PAGE_MARGIN_X)
                        .ShrinkY(PAGE_MARGIN_Y)
                )
            |+ OptionsMenuButton(
                Icons.DOWNLOAD_CLOUD + " " + %"skins.browser",
                (fun () -> SkinsBrowserPage().Show())
            )
                .Position(page_position(PAGE_BOTTOM - 4, 2, PageWidth.Full).TranslateY(-10.0f))
            |+ OptionsMenuButton(
                Icons.DOWNLOAD + " " + %"skins.import_from_osu",
                (fun () -> osu.Skins.OsuSkinsListPage().Show())
            )
                .Position(page_position(PAGE_BOTTOM - 2, 2, PageWidth.Full))

        let right_side =
            NavigationContainer.Row()
                .WrapNavigation(false)
                .Position(
                    Position
                        .ShrinkPercentL(0.35f)
                        .ShrinkL(Style.PADDING * 2.0f)
                        .Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y)
                )
            |+ noteskin_tab
            |+ hud_tab
            |+ Text(%"skins.current_noteskin")
                .Color(Colors.text_subheading)
                .Align(Alignment.LEFT)
                .Position(Position.SlicePercentL(0.5f).SliceT(PAGE_ITEM_HEIGHT * 0.65f))
            |+ Text(fun () -> Content.NoteskinMeta.Name)
                .Color(Colors.text)
                .Align(Alignment.LEFT)
                .Position(Position.SlicePercentL(0.5f).ShrinkT(PAGE_ITEM_HEIGHT * 0.5f).SliceT(PAGE_ITEM_HEIGHT))
            |+ Text(%"skins.current_hud")
                .Color(Colors.text_subheading)
                .Align(Alignment.LEFT)
                .Position(Position.SlicePercentR(0.5f).SliceT(PAGE_ITEM_HEIGHT * 0.65f))
            |+ Text(fun () -> Content.HUDMeta.Name)
                .Color(Colors.text)
                .Align(Alignment.LEFT)
                .Position(Position.SlicePercentR(0.5f).ShrinkT(PAGE_ITEM_HEIGHT * 0.5f).SliceT(PAGE_ITEM_HEIGHT))

        NavigationContainer.Row()
            .With(right_side, left_info)
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"skins"

    override this.OnReturnFromNestedPage() = refresh()