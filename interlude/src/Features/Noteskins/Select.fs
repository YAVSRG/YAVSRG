namespace Interlude.Features.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.Features.Online
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Import
open Interlude.Features.Noteskins.Edit

module private PreviewCleanup =

    let mutable private list = List.empty

    let add (s: Sprite) = list <- s :: list

    let clear () =
        list |> List.iter (Sprite.destroy >> ignore)
        list <- List.empty

type private NoteskinButton(id: string, ns: Noteskin, on_switch: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if Noteskins.selected_id.Value <> id then
                    options.Noteskin.Set id
                    Style.click.Play()
                    on_switch ()
            )
        )

    let mutable preview: Sprite option = None
    let preview_fade = Animation.Fade 0.0f

    member this.IsCurrent = Noteskins.selected_id.Value = id

    override this.Init(parent: Widget) =
        Noteskins.preview_loader.Request(
            ns,
            function
            | Some(bmp, config) ->
                defer (fun () ->
                    preview <-
                        Some(
                            Sprite.upload_one
                                false
                                true
                                {
                                    Label = "NOTESKIN_PREVIEW"
                                    Image = bmp
                                    Rows = config.Rows
                                    Columns = config.Columns
                                    DisposeImageAfter = true
                                }
                        )

                    PreviewCleanup.add preview.Value
                    bmp.Dispose()
                    preview_fade.Target <- 1.0f
                )
            | None -> ()
        )

        this
        |+ Text(
            K ns.Config.Name,
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
                let has_other_versions =
                    Noteskins.list ()
                    |> Seq.map (snd >> _.Config)
                    |> Seq.tryFind (fun cfg -> cfg.Name = ns.Config.Name && cfg.Version <> ns.Config.Version)
                    |> Option.isSome

                (if has_other_versions then
                     sprintf "%s, By %s" ns.Config.Version ns.Config.Author
                 else
                     sprintf "By %s" ns.Config.Author)
                + (
                    match ns.Config.Editor with
                    | Some e -> ", Edit by " + e
                    | None -> ""
                )
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(7.5f, Style.PADDING).SliceBottom(30.0f)
        )
        |* Clickable.Focus this

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        preview_fade.Update elapsed_ms

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        match preview with
        | Some p -> Draw.sprite (this.Bounds.SliceLeft(100.0f).Shrink(5.0f)) (Colors.white.O4a preview_fade.Alpha) p
        | None -> ()

        base.Draw()

type NoteskinsPage() =
    inherit Page()

    let preview = NoteskinPreview(NoteskinPreview.LEFT_HAND_SIDE 0.35f)

    let grid =
        GridFlowContainer<NoteskinButton>(100.0f, 2, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let rec try_edit_noteskin () =
        let ns = Content.Noteskin

        if ns.IsEmbedded then
            ConfirmPage(
                [ ns.Config.Name ] %> "noteskins.confirm_extract_default",
                (fun () ->
                    if
                        Noteskins.create_from_embedded (
                            if Network.credentials.Username <> "" then
                                Some Network.credentials.Username
                            else
                                None
                        )
                    then
                        refresh ()
                    else
                        Logging.Error "Noteskin folder already exists"
                )
            )
                .Show()
        else
            Menu.ShowPage
                { new EditNoteskinPage(false) with
                    override this.OnClose() =
                        base.OnClose()
                        refresh ()
                }

    and refresh () =
        grid.Clear()
        preview.Refresh()

        for id, noteskin in Noteskins.list () do
            grid |* NoteskinButton(id, noteskin, preview.Refresh)

    override this.Content() =
        refresh ()

        let left_side =
            NavigationContainer.Column(Position = { Position.Default with Right = 0.35f %+ PRETTY_MARGIN_X }.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
            |+ PageButton(%"noteskins.edit", try_edit_noteskin, Icon = Icons.EDIT_2)
                .Tooltip(Tooltip.Info("noteskins.edit"))
                .Pos(PAGE_BOTTOM - 4, 2, PageWidth.Full)
            |+ PageButton(%"osu_skin_import.list_skins", 
                (fun () -> osu.Skins.OsuSkinsListPage().Show()), 
                Icon = Icons.DOWNLOAD
            )
                .Pos(PAGE_BOTTOM - 2, 2, PageWidth.Full)
            |+ Text(
                %"noteskins.current",
                Position = pretty_pos(0, 1, PageWidth.Full).SliceTop(PRETTYHEIGHT * 0.65f),
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT
            )
            |+ Text(
                (fun () -> Content.NoteskinConfig.Name),
                Position = pretty_pos (1, 3, PageWidth.Full),
                Color = K Colors.text,
                Align = Alignment.LEFT
            )
            |>> Container
            |+ preview

        let right_side =
            NavigationContainer.Column(
                Position =
                    { Position.Default with
                        Left = 0.35f %+ 50.0f
                    }
                        .Margin(PRETTY_MARGIN_X, 0.0f)
                        .TrimTop(150.0f)
                        .TrimBottom(50.0f)
            )
            |+ (GridFlowContainer(PRETTYHEIGHT, 2, WrapNavigation = false, Position = Position.SliceTop(PRETTYHEIGHT))
                |+ PageButton(
                    %"noteskins.get_more",
                    (fun () -> NoteskinsBrowserPage().Show())
                )
                |+ PageButton(%"noteskins.open_folder", (fun () -> open_directory (get_game_folder "Noteskins")))
                    .Tooltip(Tooltip.Info("noteskins.open_folder")))
            |+ ScrollContainer(grid, Position = Position.TrimTop(PRETTYHEIGHT * 1.5f))

        NavigationContainer.Row() 
        |+ left_side
        |+ right_side
        :> Widget

    override this.Title = %"noteskins"

    override this.OnDestroy() =
        preview.Destroy()
        PreviewCleanup.clear ()

    override this.OnClose() = ()
