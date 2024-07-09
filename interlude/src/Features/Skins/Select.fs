namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins
open Interlude.Content
open Interlude.Features.Online
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Import
open Interlude.Features.Gameplay
open Interlude.Features.Skins.EditNoteskin
open Interlude.Features.Skins.EditHUD
open Interlude.Features.Skins.Browser

type private NoteskinButton(id: string, meta: SkinMetadata, on_switch: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if Skins.selected_noteskin_id.Value <> id then
                    options.Noteskin.Set id
                    Style.click.Play()
                    on_switch ()
            )
        )

    member this.IsCurrent = Skins.selected_noteskin_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K meta.Name,
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2
                    elif this.IsCurrent then Colors.text_pink
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(Style.PADDING).SliceTop(70.0f)
        )
        |+ Text(
            K(
                match meta.Editor with
                | Some e -> [meta.Author; e] %> "skins.credit.edited"
                | None -> [meta.Author] %> "skins.credit"
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(7.5f, Style.PADDING).SliceBottom(30.0f)
        )
        |* Clickable.Focus this

        match Skins.get_icon id with
        | Some sprite -> this.Add(Image(sprite, Position = Position.SliceLeft(100.0f).Margin(Style.PADDING)))
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type private HUDButton(id: string, meta: SkinMetadata, on_switch: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if Skins.selected_hud_id.Value <> id then
                    options.SelectedHUD.Set id
                    Style.click.Play()
                    on_switch ()
            )
        )

    member this.IsCurrent = Skins.selected_hud_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K meta.Name,
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2
                    elif this.IsCurrent then Colors.text_green
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(Style.PADDING).SliceTop(70.0f)
        )
        |+ Text(
            K(
                match meta.Editor with
                | Some e -> [meta.Author; e] %> "skins.credit.edited"
                | None -> [meta.Author] %> "skins.credit"
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(7.5f, Style.PADDING).SliceBottom(30.0f)
        )
        |* Clickable.Focus this

        match Skins.get_icon id with
        | Some sprite -> this.Add(Image(sprite, Position = Position.SliceLeft(100.0f).Margin(Style.PADDING)))
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Draw.rect this.Bounds Colors.green_accent.O1
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type SelectSkinsPage() =
    inherit Page()

    let preview = SkinPreview(SkinPreview.LEFT_HAND_SIDE 0.35f)

    let noteskin_grid =
        GridFlowContainer<NoteskinButton>(100.0f, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let hud_grid =
        GridFlowContainer<HUDButton>(100.0f, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let refresh () =
        preview.Refresh()

        noteskin_grid.Clear()
        for id, _, meta in Skins.list_noteskins () do
            noteskin_grid |* NoteskinButton(id, meta, preview.Refresh)

        hud_grid.Clear()
        for id, _, meta in Skins.list_huds () do
            hud_grid |* HUDButton(id, meta, preview.Refresh)

    let edit_or_extract_noteskin () =
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
                        |> not
                    then
                        Logging.Error "An editable skin has already been extracted"
                )
            )
                .Show()
        else EditNoteskinPage(false).Show()

    override this.Content() =
        refresh ()

        let left_info =
            NavigationContainer.Column(Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.35f %- 10.0f })
            |+ OptionsMenuButton(
                Icons.DOWNLOAD_CLOUD + " " + %"skins.browser",
                0.0f,
                (fun () -> SkinsBrowserPage().Show()),
                Position = pretty_pos(PAGE_BOTTOM - 4, 2, PageWidth.Full).Translate(0.0f, -10.0f)
            )
            |+ OptionsMenuButton(
                Icons.DOWNLOAD + " " + %"skins.import_from_osu",
                0.0f,
                (fun () -> osu.Skins.OsuSkinsListPage().Show()),
                Position = pretty_pos(PAGE_BOTTOM - 2, 2, PageWidth.Full)
            )
            |+ Text(
                %"skins.current",
                Position = pretty_pos(0, 1, PageWidth.Full).SliceTop(PRETTYHEIGHT * 0.65f),
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT
            )
            |+ Text(
                (fun () -> Content.NoteskinMeta.Name),
                Position = pretty_pos (1, 3, PageWidth.Full),
                Color = K Colors.text,
                Align = Alignment.LEFT
            )
            |+ Text(
                (fun () -> if Skins.selected_hud_id.Value <> Skins.selected_noteskin_id.Value then "HUD: " + Content.HUDMeta.Name else ""),
                Position = pretty_pos(3, 2, PageWidth.Full).TrimTop(15.0f),
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT
            )

        let noteskin_tab = ScrollContainer(noteskin_grid)
        let hud_tab = ScrollContainer(hud_grid)
        let tabs = SwapContainer(noteskin_tab, Position = Position.TrimTop(115.0f))

        let tab_buttons =
            let c =
                RadioButtons.create_tabs
                    {
                        Setting = Setting.make tabs.set_Current tabs.get_Current
                        Options =
                            [|
                                noteskin_tab, %"skins.noteskins", K false
                                hud_tab, %"skins.huds", K false
                            |]
                        Height = 50.0f
                    }
            c.Position <- Position.TrimTop(60.0f).SliceTop(50.0f)
            c

        let action_buttons =
            GridFlowContainer<Widget>(70.0f, 4, Spacing = (0.0f, 0.0f), Position = Position.SliceTop(60.0f), WrapNavigation = false)
            |+ InlaidButton(
                %"skins.edit",
                (fun () ->
                    if tabs.Current = noteskin_tab then
                        edit_or_extract_noteskin()
                    elif
                        SelectedChart.WITH_COLORS.IsSome
                        && Screen.change_new
                            (fun () -> EditHudScreen.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, fun () -> SelectSkinsPage().Show()))
                            Screen.Type.Practice
                            Transitions.Default
                    then
                        Menu.Exit()
                ),
                Icons.EDIT
            )
            |+ InlaidButton(
                %"skins.export",
                (fun () -> 
                    if tabs.Current = noteskin_tab then
                        if not (Skins.export_skin Skins.selected_noteskin_id.Value || Skins.current_noteskin.IsEmbedded) then
                            Notifications.error (
                                %"notification.export_skin_failure.title",
                                %"notification.export_skin_failure.body"
                            )
                    else
                        if not (Skins.export_skin Skins.selected_hud_id.Value) then
                            Notifications.error (
                                %"notification.export_skin_failure.title",
                                %"notification.export_skin_failure.body"
                            )
                ),
                Icons.UPLOAD
            )
                .Help(Help.Info("skins.export"))
            |+ InlaidButton(
                %"skins.open_folder",
                (fun () -> 
                    if tabs.Current = noteskin_tab then
                        Skins.open_noteskin_folder Skins.selected_noteskin_id.Value |> ignore
                    else
                        Skins.open_hud_folder Skins.selected_hud_id.Value |> ignore
                ),
                Icons.FOLDER
            )
            |+ InlaidButton(
                %"skins.delete",
                (fun () -> 
                    if tabs.Current = noteskin_tab then
                        ConfirmPage([Content.NoteskinMeta.Name] %> "noteskin.delete.confirm",
                            fun () -> Skins.delete_noteskin Skins.selected_noteskin_id.Value |> ignore
                        ).Show()
                    else
                        ConfirmPage([Content.HUDMeta.Name] %> "hud.delete.confirm",
                            fun () -> Skins.delete_hud Skins.selected_hud_id.Value |> ignore
                        ).Show()
                ),
                Icons.TRASH
            )

        let right_side =
            NavigationContainer.Column(
                WrapNavigation = false, 
                Position =
                    { Position.Default with
                        Left = 0.35f %+ 10.0f
                    }
                        .Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)
            )
            |+ action_buttons
            |+ tab_buttons
            |+ tabs

        NavigationContainer.Row()
        |+ right_side
        |+ left_info
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"skins"

    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh()
