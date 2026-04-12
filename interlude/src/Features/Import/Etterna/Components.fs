namespace Interlude.Features.Import.Etterna

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

type private PackDownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type EtternaPackCard(data: EtternaOnlinePack) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            ),
            Fill = (fun () -> if this.Focused then Color.FromArgb(0xFF_8F60F6) else Color.FromArgb(0xFF_8F60F6).O2),
            Border = (fun () -> if this.Focused then Color.White else Color.FromArgb(0xFF_8F60F6))
        )

    let mutable progress = 0.0f

    // todo: check if a chart exists with this pack as its source
    let mutable status = NotDownloaded

    let download () =
        if status <> Downloading then
            progress <- 0.0f

            let task_tracking = TaskTracking.add data.name
            let progress_callback = fun p ->
                match p with
                | Data.Downloading percent -> progress <- percent
                | _ -> ()
                task_tracking.Progress <- p

            let task = OnlineImports.download_etterna_pack(data.name, data.download, Content.Charts, Content.UserData, progress_callback)
            import_queue.Request(task,
                function
                | Ok result ->
                    Notifications.task_feedback (
                        Icons.DOWNLOAD,
                        %"notification.install_pack",
                        [data.name; result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.install_pack.body"
                    )
                    Content.TriggerChartAdded()
                    progress <- 1.0f
                    status <- Installed
                | Error reason ->
                    Logging.Error "Error installing %s: %s" data.name reason
                    Notifications.error (%"notification.install_pack_failed", data.name)
                    status <- DownloadFailed
            )

            status <- Downloading
        
    static member HEIGHT = 80.0f

    override this.Init(parent: Widget) =
        this
            .With(
                Text(data.name)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(45.0f).ShrinkX(Style.PADDING * 2.0f)),
                Text(data.size)
                    .Align(Alignment.RIGHT)
                    .Position(Position.SliceT(45.0f).ShrinkR(EtternaPackCard.HEIGHT * 2.0f + Style.PADDING).ShrinkX(Style.PADDING * 2.0f)),
                // todo: indicate if pack is already downloaded
                // Text(fun () ->
                //     if status = Installed then "Downloaded!"
                //     elif status = DownloadFailed then "Download failed!"
                //     else ""
                // )
                //     .Color(fun () ->
                //         if status = DownloadFailed then Colors.text_red
                //         else Colors.text_green
                //     )
                //     .Align(Alignment.RIGHT)
                //     .Position(Position.SliceB(45.0f).ShrinkR(HEIGHT * 2.0f + Style.PADDING).Shrink(Style.PADDING * 2.0f, Style.PADDING)),
                Text([sprintf "%.2f" data.overall] %> "etterna_pack_browser.difficulty")
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceB(45.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING)),
                FrameContainer.Create(
                    Button((fun () ->
                        match status with
                        | NotDownloaded -> Icons.DOWNLOAD
                        | Downloading -> Icons.REFRESH_CW
                        | DownloadFailed -> Icons.X
                        | Installed -> Icons.CHECK
                        ),
                        download
                    )
                        .TextColor(fun () ->
                            match status with
                            | NotDownloaded -> Colors.text
                            | Downloading -> Colors.text_yellow_2
                            | DownloadFailed -> Colors.text_red
                            | Installed -> Colors.text_green
                        )
                        .Position(Position.Shrink(Style.PADDING * 2.0f))
                )
                    .Fill(Colors.shadow_2.O2)
                    .Border(Color.Transparent)
                    .Position(Position.SliceR(EtternaPackCard.HEIGHT).Shrink(Style.PADDING * 2.0f)),
                MouseListener().Button(this),
                FrameContainer.Create(
                    Button(Icons.EXTERNAL_LINK, fun () -> open_url (sprintf "https://etternaonline.com/packs/%i" data.id))
                        .Position(Position.Shrink(Style.PADDING * 2.0f))
                )
                    .Fill(Colors.shadow_2.O2)
                    .Border(Color.Transparent)
                    .Position(Position.SliceR(EtternaPackCard.HEIGHT, EtternaPackCard.HEIGHT).Shrink(Style.PADDING * 2.0f))
            )
            .AddConditional(
                data.contains_nsfw,
                Text(Icons.ALERT_TRIANGLE + " " + %"etterna_pack_browser.nsfw_warning")
                    .Color(Colors.text_red)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(45.0f).Shrink(10.0f, 5.0f).Translate(15.0f + Text.measure (Style.font, data.name) * 45.0f * 0.6f, 0.0f))
            )

        base.Init(parent)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        if progress > 0.0f && status = Downloading then
            Render.rect (this.Bounds.SliceL(this.Bounds.Width * progress)) Colors.white.O1

    member this.Data = data

    member private this.Download() = download ()

    static member Filter(filter: FilterPart list) =
        fun (c: EtternaPackCard) ->
            List.forall
                (function
                | Impossible -> false
                | String str -> c.Data.name.ToLower().Contains(str)
                | _ -> true)
                filter