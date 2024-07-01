namespace Interlude.Features.Noteskins.Browser

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Repo
open Prelude.Data
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content

type private Status =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type VersionDisplay(group: NoteskinGroup, version: NoteskinVersion) as this =
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

    let folder_when_downloaded = group.Name + "_" + version.Version

    let mutable status =
        if
            Skins.list_noteskins ()
            |> Seq.map (fun (id, _, _) -> id)
            |> Seq.contains folder_when_downloaded
        then
            Installed
        else
            NotDownloaded

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
        |+ { new Thumbnail(Position = Position.TrimTop(130.0f).Margin(10.0f)) with
            override this.Load() =
                ImageServices.get_cached_image.Request(
                    version.Preview,
                    function
                    | Some img -> defer (fun () -> this.FinishLoading img)
                    | None -> Logging.Warn("Failed to load noteskin preview", version.Preview)
                )
        }
        |* Clickable.Focus this
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Download() =
        if status = NotDownloaded || status = DownloadFailed then
            ConfirmPage([title] %> "noteskin.confirm_install", fun () ->
                status <- Downloading

                let target =
                    Path.Combine(
                        get_game_folder "Skins",
                        folder_when_downloaded + ".isk"
                    )

                WebServices.download_file.Request(
                    (version.Download, target, ignore),
                    fun success ->
                        if success then
                            defer Skins.load
                            Notifications.task_feedback (Icons.DOWNLOAD, %"notification.install_noteskin", group.Name)
                            status <- Installed
                        else
                            status <- DownloadFailed
                )
            ).Show()

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

type GroupDisplay(group: NoteskinGroup, selected: Setting<bool>) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                selected.Set true
            )
        )

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
        |+ { new Thumbnail(Position = Position.SliceLeft(100.0f).Margin(Style.PADDING)) with
            override this.Load() =
                ImageServices.get_cached_image.Request(
                    group.Thumbnail,
                    function
                    | Some img -> defer (fun () -> this.FinishLoading img)
                    | None -> Logging.Warn("Failed to load noteskin thumbnail", group.Thumbnail)
                )
        }
        |* Clickable.Focus this
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if selected.Value then
            Draw.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

    member this.Name = group.Name

    static member Filter(query: string) =
        fun (c: GroupDisplay) ->
            c.Name.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)