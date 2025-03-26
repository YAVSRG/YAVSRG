namespace Interlude.Features.Skins.Browser

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Repo
open Prelude.Data
open Interlude.UI
open Interlude.Content

type private Status =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type VersionDisplay(group: SkinGroup, version: SkinVersion) as this =
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

    let thumbnail =
        { new Thumbnail() with
            override this.Load() =
                ImageServices.get_cached_image.Request(
                    version.Preview,
                    function
                    | Some img -> GameThread.defer (fun () -> this.FinishLoading img)
                    | None -> Logging.Warn "Failed to load noteskin preview '%s'" version.Preview
                )
        }

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
        |+ Text(title)
            .Color(fun () ->
                if this.Focused then
                    Colors.text_yellow_2
                else
                    Colors.text
            )
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(0.0f, 70.0f).Shrink(Style.PADDING))
        |+ Text(
            match version.Editor with
            | Some e -> [version.Author; e] %> "skins.credit.edited"
            | None -> [version.Author] %> "skins.credit"
        )
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(60.0f, 50.0f).Shrink(Style.PADDING))
        |+ thumbnail
            .Position(Position.ShrinkT(130.0f).Shrink(10.0f))
        |* MouseListener().Button(this)
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Download() =
        if status = NotDownloaded || status = DownloadFailed then
            ConfirmPage([title] %> "skins.confirm_install", fun () ->
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
                            GameThread.defer Skins.load
                            Notifications.task_feedback (Icons.DOWNLOAD, %"notification.install_skin", group.Name)
                            status <- Installed
                        else
                            status <- DownloadFailed
                )
            ).Show()

    override this.Draw() =
        if this.Focused then Render.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

type GroupDisplay(group: SkinGroup, selected: Setting<bool>) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                selected.Set true
            )
        )

    let thumbnail =
        { new Thumbnail() with
            override this.Load() =
                ImageServices.get_cached_image.Request(
                    group.Thumbnail,
                    function
                    | Some img -> GameThread.defer (fun () -> this.FinishLoading img)
                    | None -> Logging.Warn "Failed to load noteskin thumbnail '%s'" group.Thumbnail
                )
        }

    let subtitle =
        match List.tryExactlyOne group.Versions with
        | Some version ->
            match version.Editor with
            | Some e -> [version.Author; e] %> "skins.credit.edited"
            | None -> [version.Author] %> "skins.credit"
        | None -> %"skins.multiple_versions"

    override this.Init(parent) =
        this
        |+ Text(group.Name)
            .Color(fun () ->
                if this.Focused then Colors.text_yellow_2
                elif selected.Value then Colors.text_pink
                else Colors.text
            )
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(100.0f).Shrink(Style.PADDING).SliceT(70.0f))
        |+ Text(subtitle)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(100.0f).Shrink(7.5f, Style.PADDING).SliceB(30.0f))
        |+ thumbnail
            .Position(Position.SliceL(100.0f).Shrink(Style.PADDING))
        |* MouseListener().Button(this)
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if selected.Value then
            Render.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

    member this.Name = group.Name
    member this.Versions = group.Versions

    static member Filter(query: string) =
        fun (c: GroupDisplay) ->
            c.Name.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)
            || c.Versions |> Seq.exists(fun v -> v.Version.Contains(query, System.StringComparison.InvariantCultureIgnoreCase))