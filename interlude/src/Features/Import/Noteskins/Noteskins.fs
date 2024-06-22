namespace Interlude.Features.Import

open System.IO
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins.Repo
open Prelude.Data
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content

type private NoteskinDownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type NoteskinVersionCard(group: NoteskinGroup, version: NoteskinVersion) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            )
        )

    let title = 
        if group.Versions.Length = 1 then
            group.Name
        else
            group.Name + " - " + version.Version

    let mutable status =
        if
            Noteskins.list ()
            |> Seq.map (snd >> _.Config)
            |> Seq.tryFind (fun cfg -> cfg.Name = group.Name && cfg.Version = version.Version)
            |> Option.isSome
        then
            Installed
        else
            NotDownloaded

    let mutable preview: Sprite option = None
    let preview_fade = Animation.Fade 0.0f

    override this.Init(parent) =
        this
        |+ Text(
            title,
            Color =
                (fun () ->
                    if this.Focused then
                        Colors.text_yellow_2
                    else
                        Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.Row(0.0f, 70.0f).Margin(Style.PADDING)
        )
        |+ Text(
            (
                match version.Editor with
                | Some e -> [version.Author; e] %> "noteskin.credit.edited"
                | None -> [version.Author] %> "noteskin.credit"
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.Row(60.0f, 50.0f).Margin(Style.PADDING)
        )
        |* Clickable.Focus this
        base.Init parent

        ImageServices.get_cached_image.Request(
            version.Preview,
            function
            | Some img -> defer (fun () -> this.LoadPreview img)
            | None -> Logging.Warn("Failed to load noteskin preview", version.Preview)
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Download() =
        if status = NotDownloaded || status = DownloadFailed then
            ConfirmPage([title] %> "noteskin.confirm_install", fun () ->
                status <- Downloading

                let target =
                    Path.Combine(
                        get_game_folder "Noteskins",
                        Regex("[^a-zA-Z0-9_-]").Replace(title, "")
                        + ".isk"
                    )

                WebServices.download_file.Request(
                    (version.Download, target, ignore),
                    fun success ->
                        if success then
                            defer Noteskins.load
                            Notifications.task_feedback (Icons.DOWNLOAD, %"notification.install_noteskin", group.Name)
                            status <- Installed
                        else
                            status <- DownloadFailed
                )
            ).Show()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        preview_fade.Update elapsed_ms

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds Colors.shadow_2.O2

        base.Draw()

        match preview with
        | Some p ->
            let img_bounds = Sprite.fill (this.Bounds.TrimTop(130.0f).Shrink(10.0f)) p
            Draw.sprite img_bounds (Colors.white.O4a preview_fade.Alpha) p
        | None -> ()

    member this.LoadPreview(img: Bitmap) =
        preview <-
            Some
            <| Sprite.upload_one false true (SpriteUpload.OfImage("NOTESKIN_PREVIEW", img))

        preview_fade.Target <- 1.0f

type NoteskinGroupCard(group: NoteskinGroup, selected: Setting<bool>) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                selected.Set true
            )
        )

    let mutable preview: Sprite option = None
    let preview_fade = Animation.Fade 0.0f

    let subtitle =
        match List.tryExactlyOne group.Versions with
        | Some version ->
            match version.Editor with
            | Some e -> [version.Author; e] %> "noteskin.credit.edited"
            | None -> [version.Author] %> "noteskin.credit"
        | None -> %"noteskin.multiple_versions"

    override this.Init(parent) =
        this
        |+ Text(
            group.Name,
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2
                    elif selected.Value then Colors.text_pink
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(Style.PADDING).SliceTop(70.0f)
        )
        |+ Text(
            subtitle,
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimLeft(100.0f).Margin(7.5f, Style.PADDING).SliceBottom(30.0f)
        )
        |* Clickable.Focus this
        base.Init parent

        ImageServices.get_cached_image.Request(
            group.Thumbnail,
            function
            | Some img -> defer (fun () -> this.LoadPreview img)
            | None -> Logging.Warn("Failed to load noteskin preview", group.Thumbnail)
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        preview_fade.Update elapsed_ms

    override this.Draw() =
        if selected.Value then
            Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

        match preview with
        | Some p -> Draw.sprite (this.Bounds.SliceLeft(100.0f).Shrink(5.0f)) (Colors.white.O4a preview_fade.Alpha) p
        | None -> ()

    member this.LoadPreview(img: Bitmap) =
        preview <-
            Some
            <| Sprite.upload_one false true (SpriteUpload.OfImage("NOTESKIN_THUMBNAIL", img))

        preview_fade.Target <- 1.0f

    member this.Name = group.Name

    static member Filter(query: string) =
        fun (c: NoteskinGroupCard) ->
            c.Name.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)

module Noteskins =

    let tab = Dummy(NodeType.Leaf)

type InstallNoteskinsPage() =
    inherit Page()

    let mutable loading = true
    let mutable error = false
    let mutable selected_group = None

    let noteskin_items =
        GridFlowContainer<NoteskinGroupCard>(100.0f, 2, WrapNavigation = false, Spacing = (15.0f, 15.0f))

    let version_items =
        FlowContainer.Vertical<NoteskinVersionCard>(520.0f, Spacing = 15.0f)

    let search_groups = 
        NavigationContainer.Column()
        |+ Dummy(NodeType.Leaf)
        |+ ScrollContainer(noteskin_items, Margin = Style.PADDING, Position = Position.TrimTop(70.0f))
        |>> (fun nt -> Container(nt, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.65f %- 10.0f }))
        |+ (SearchBox(
                Setting.simple "",
                (fun (query: string) -> noteskin_items.Filter <- NoteskinGroupCard.Filter query),
                Position = Position.SliceTop 60.0f,
                Fill = K Colors.cyan.O3,
                Border = K Colors.cyan_accent,
                TextColor = K Colors.text_cyan
            )
            |+ LoadingIndicator.Border(fun () -> loading))
        |+ EmptyState(Icons.X, %"imports.noteskins.error").Conditional(fun () -> error)

    let pick_versions =
        ScrollContainer(version_items, Margin = Style.PADDING, Position = Position.TrimTop(70.0f))
        |>> (fun nt -> Container(nt, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Left = 0.65f %+ 10.0f }))
        |+ Text(%"imports.noteskins.install_hint", Color = K Colors.text_subheading, Align = Alignment.CENTER, Position = Position.SliceTop(70.0f).Margin(10.0f)).Conditional(fun () -> selected_group.IsSome)

    let select_group(group: NoteskinGroup) =
        selected_group <- Some group
        version_items.Clear()
        for version in group.Versions do
            version_items.Add <| NoteskinVersionCard(group, version)

    member this.Refresh() =
        loading <- true
        error <- false
        noteskin_items.Clear()
        WebServices.download_json (
            sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/backbeat/noteskins/index.json" "githubusercontent",
            fun data ->
                match data with
                | Some(d: NoteskinRepo) ->
                    defer (fun () ->
                        for group in d.Noteskins do
                            let is_selected = Setting.make (fun _ -> select_group group) (fun _ -> selected_group = Some group)
                            noteskin_items.Add <| NoteskinGroupCard(group, is_selected)
                        loading <- false
                    )
                | None ->
                    defer (fun () ->
                        error <- true
                        loading <- false
                    )
        )

    override this.Content() =

        this.Refresh()

        NavigationContainer.Row()
        |+ search_groups
        |+ pick_versions
        :> Widget

    override this.Title = %"imports.noteskins"

    override this.OnClose() = ()