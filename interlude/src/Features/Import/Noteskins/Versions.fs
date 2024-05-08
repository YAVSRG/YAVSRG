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

type NoteskinVersionCard(group: NoteskinGroup, version: NoteskinVersion) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            ),
            Fill = K Colors.shadow_2.O2,
            Border =
                (fun () ->
                    if this.Focused then
                        Colors.pink_accent
                    else
                        Colors.grey_2.O3
                )
        )

    let is_the_only_version = group.Versions.Length = 1

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

    do
        this
        |+ Text(
            (if is_the_only_version then group.Name else version.Version),
            Align = Alignment.CENTER,
            Position = Position.SliceLeft(400.0f).SliceTop(70.0f).Margin(Style.PADDING)
        )
        |+ Text(
            (match version.Editor with
             | Some e -> "Edit by " + e
             | None -> "By " + group.Author),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER,
            Position = Position.SliceLeft(400.0f).TrimTop(65.0f).SliceTop(55.0f).Margin(Style.PADDING)
        )
        |* Clickable.Focus this

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Download() =
        if status = NotDownloaded || status = DownloadFailed then
            status <- Downloading

            let target =
                Path.Combine(
                    get_game_folder "Noteskins",
                    Regex("[^a-zA-Z0-9_-]").Replace(group.Name, "")
                    + (if is_the_only_version then
                           ""
                       else
                           "-" + Regex("[^a-zA-Z0-9_-]").Replace(version.Version, ""))
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

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        preview_fade.Update elapsed_ms

    override this.Draw() =

        base.Draw()

        match preview with
        | Some p ->
            let img_bounds =
                Rect.Box(this.Bounds.Left + 420.0f, this.Bounds.Top + 20.0f, 640.0f, 480.0f)

            Draw.sprite img_bounds (Colors.white.O4a preview_fade.Alpha) p
        | None -> ()

        Draw.rect (this.Bounds.SliceLeft(400.0f).SliceBottom(70.0f).Shrink(50.0f, 10.0f)) Colors.shadow_2.O2

        Text.fill_b (
            Style.font,
            (match status with
             | NotDownloaded -> Icons.DOWNLOAD + " Download"
             | Downloading -> Icons.DOWNLOAD + " Downloading .."
             | DownloadFailed -> Icons.X + " Error"
             | Installed -> Icons.CHECK + " Downloaded"),
            this.Bounds.SliceLeft(400.0f).SliceBottom(70.0f).Shrink(50.0f, 10.0f),
            (match status with
             | NotDownloaded -> if this.Focused then Colors.text_yellow_2 else Colors.text
             | Downloading -> Colors.text_yellow_2
             | DownloadFailed -> Colors.text_red
             | Installed -> Colors.text_green),
            Alignment.CENTER
        )

    member this.LoadPreview(img: Bitmap) =
        preview <-
            Some
            <| Sprite.upload_one false true (SpriteUpload.OfImage("NOTESKIN_PREVIEW", img))

        preview_fade.Target <- 1.0f

type NoteskinGroupPage(group: NoteskinGroup) =
    inherit Page()

    override this.Init(parent: Widget) =

        let flow = FlowContainer.Vertical<NoteskinVersionCard>(520.0f, Spacing = 30.0f)

        for version in group.Versions do
            let nc = NoteskinVersionCard(group, version)

            ImageServices.get_cached_image.Request(
                version.Preview,
                function
                | Some img -> defer (fun () -> nc.LoadPreview img)
                | None -> Logging.Warn("Failed to load noteskin preview", version.Preview)
            )

            flow.Add nc

        ScrollContainer(
            flow,
            Margin = Style.PADDING,
            Position =
                {
                    Left = 0.5f %- 540.0f
                    Right = 0.5f %+ 540.0f
                    Top = 0.0f %+ 200.0f
                    Bottom = 1.0f %- 0.0f
                }
        )
        |> this.Content

        this
        |* Text(
            "By " + group.Author,
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimTop(80.0f).SliceTop(50.0f).Margin(20.0f, 0.0f)
        )

        base.Init parent

    override this.Title = group.Name
    override this.OnClose() = ()