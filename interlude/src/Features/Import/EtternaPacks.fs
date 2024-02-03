namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

[<Json.AutoCodec>]
type EtternaOnlinePackAttributes =
    {
        name: string
        average: float
        download: string
        mirror: string
        size: int64
    }

[<Json.AutoCodec>]
type EtternaOnlinePack =
    {
        ``type``: string
        id: int
        attributes: EtternaOnlinePackAttributes
    }

type EtternaPackCard(id: int, data: EtternaOnlinePackAttributes) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            ),
            Fill = (fun () -> Palette.color (120, 0.5f, 0.0f)),
            Border =
                (fun () ->
                    if this.Focused then
                        Color.White
                    else
                        Palette.color (200, 0.7f, 0.2f)
                )
        )

    let mutable progress = 0.0f

    let mutable status =
        let path = Path.Combine(get_game_folder "Songs", data.name)

        if Directory.Exists path && not (Seq.isEmpty (Directory.EnumerateDirectories path)) then
            Installed
        else
            NotDownloaded

    let download () =
        if status = NotDownloaded || status = DownloadFailed then
            let target =
                Path.Combine(get_game_folder "Downloads", System.Guid.NewGuid().ToString() + ".zip")

            WebServices.download_file.Request(
                (data.download, target, (fun p -> progress <- p)),
                fun completed ->
                    if completed then
                        Library.Imports.convert_stepmania_pack_zip.Request(
                            (target, id),
                            fun b ->
                                if b then
                                    charts_updated_ev.Trigger()

                                Notifications.task_feedback (Icons.DOWNLOAD, %"notification.install_pack", data.name)
                                File.Delete target
                                status <- if b then Installed else DownloadFailed
                        )
                    else
                        status <- DownloadFailed
            )

            status <- Downloading

    do
        this
        |+ Text(
            data.name,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = Position.min
                    Right = 1.0f %- 400.0f
                    Bottom = 1.0f %- 30.0f
                }
        )
        |+ Text(
            (sprintf "%.1fMB" (float data.size / 1000000.0)),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = Position.min
                    Right = 1.0f %- 165.0f
                    Bottom = 1.0f %- 30.0f
                }
        )
        |+ Text(
            (fun () ->
                if status = Installed then "Downloaded!"
                elif status = DownloadFailed then "Download failed!"
                else ""
            ),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = 0.0f %+ 50.0f
                    Right = 1.0f %- 165.0f
                    Bottom = Position.max
                }
        )
        |+ Text(
            (sprintf "Average difficulty (MSD): %.2f" data.average),
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = 0.0f %+ 50.0f
                    Right = 1.0f %- 400.0f
                    Bottom = Position.max
                }
        )
        |+ Button(
            Icons.EXTERNAL_LINK
            , fun () -> open_url (sprintf "https://etternaonline.com/pack/%i" id)
            , Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f)
        )
        |* Button(Icons.DOWNLOAD, download, Position = Position.SliceRight(80.0f).Margin(5.0f, 10.0f))

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        base.Draw()

        match status with
        | NotDownloaded -> ()
        | Downloading ->
            Draw.rect (this.Bounds.SliceLeft(this.Bounds.Width * progress)) (Color.FromArgb(64, 255, 255, 255))
        | Installed -> Draw.rect this.Bounds (Color.FromArgb(64, 255, 255, 255))
        | DownloadFailed -> ()

    member this.Data = data

    member private this.Download() = download ()

    static member Filter(filter: Filter) =
        fun (c: EtternaPackCard) ->
            List.forall
                (function
                | Impossible -> false
                | String str -> c.Data.name.ToLower().Contains(str)
                | _ -> true)
                filter

module EtternaPacks =

    // todo: automated test to ping EO and see if the cert is expired
    //let allow_expired_etternaonline_cert () =
    //    ServicePointManager.ServerCertificateValidationCallback <-
    //        RemoteCertificateValidationCallback(fun _ cert _ sslPolicyErrors ->
    //            if sslPolicyErrors = SslPolicyErrors.None then
    //                true
    //            else
    //                let cert_string = cert.GetCertHashString().ToUpper()

    //                Logging.Debug(
    //                    sprintf "Expired certificate: %s (expired on %s)" cert_string (cert.GetExpirationDateString())
    //                )

    //                cert_string = "56726C10C603AFE9C338966ABC303D161072FEE5"
    //        )

    type EtternaPackSearch() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))

        let flow = FlowContainer.Vertical<EtternaPackCard>(80.0f, Spacing = 15.0f)

        let scroll =
            ScrollContainer(flow, Margin = Style.PADDING, Position = Position.TrimTop(70.0f).TrimBottom(65.0f))

        let mutable failed = false

        override this.Init(parent) =
            allow_expired_etternaonline_cert ()

            WebServices.download_json (
                "https://api.etternaonline.com/v2/packs/",
                fun data ->
                    match data with
                    | Some(d:
                        {|
                            data: ResizeArray<EtternaOnlinePack>
                        |}) ->
                        sync (fun () ->
                            for p in d.data do
                                flow.Add(EtternaPackCard(p.id, p.attributes))
                        )
                    | None -> failed <- true
            )

            this
            |+ (SearchBox(
                Setting.simple "",
                (fun (f: Filter) -> flow.Filter <- EtternaPackCard.Filter f),
                Position = Position.SliceTop 60.0f
            ))
            |+ Text(%"imports.disclaimer.etterna", Position = Position.SliceBottom 55.0f)
            |* scroll

            base.Init parent

        override this.Focusable = flow.Focusable

        member this.Items = flow

    let tab = EtternaPackSearch()
