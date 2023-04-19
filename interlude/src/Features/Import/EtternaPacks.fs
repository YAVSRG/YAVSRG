namespace Interlude.Features.Import

open System.Net
open System.Net.Security
open System.IO
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data
open Interlude.Utils
open Interlude.UI
open Interlude.Features.LevelSelect

[<Json.AutoCodec>]
type EOPackAttrs =
    {
        name: string
        average: float
        download: string
        mirror: string
        size: int64
    }

[<Json.AutoCodec>]
type EOPack =
    {
        ``type``: string
        id: int
        attributes: EOPackAttrs
    }

type private SMImportCard(id: int, data: EOPackAttrs) as this =
    inherit Frame(NodeType.Button(fun () -> this.Download()),
        Fill = (fun () -> Style.color(120, 0.5f, 0.0f)),
        Border = (fun () -> if this.Focused then Color.White else Style.color(200, 0.7f, 0.2f))
    )
    
    let mutable progress = 0.0f
    let mutable status = 
        let path = Path.Combine(getDataPath "Songs", data.name)
        if Directory.Exists path && not ( Seq.isEmpty (Directory.EnumerateDirectories path) ) then
            Installed else NotDownloaded

    let download() =
        if status = NotDownloaded || status = DownloadFailed then
            let target = Path.Combine(getDataPath "Downloads", System.Guid.NewGuid().ToString() + ".zip")
            WebServices.download_file.Request((data.mirror, target, fun p -> progress <- p),
                fun completed ->
                    if completed then Library.Imports.auto_convert.Request(target,
                        fun b ->
                            if b then LevelSelect.refresh_all()
                            Notifications.task_feedback (Icons.download, L"notification.install_pack", data.name)
                            File.Delete target
                            status <- if b then Installed else DownloadFailed
                    )
                    else status <- DownloadFailed
                )
            status <- Downloading

    do
        this
        |+ Text(data.name,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f })
        |+ Text(
            (sprintf "%.1fMB" (float data.size / 1000000.0)),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 165.0f; Bottom = 1.0f %- 30.0f })
        |+ Text(
            (fun () -> if status = Installed then "Downloaded!" elif status = DownloadFailed then "Download failed!" else ""),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 165.0f; Bottom = Position.max })
        |+ Text(
            (sprintf "Average difficulty (MSD): %.2f" data.average),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 400.0f; Bottom = Position.max })
        |+ Button(Icons.open_in_browser,
            fun () -> openUrl(sprintf "https://etternaonline.com/pack/%i" id)
            ,
            Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))
        |* Button(Icons.download, download,
            Position = Position.SliceRight(80.0f).Margin(5.0f, 10.0f))

    override this.Draw() =
        base.Draw()

        match status with
        | NotDownloaded -> ()
        | Downloading -> Draw.rect(this.Bounds.SliceLeft(this.Bounds.Width * progress)) (Color.FromArgb(64, 255, 255, 255))
        | Installed -> Draw.rect this.Bounds (Color.FromArgb(64, 255, 255, 255))
        | DownloadFailed -> ()

    member this.Data = data

    member private this.Download() = download()

    static member Filter(filter: Filter) =
        fun (c: Widget) ->
            match c with
            | :? SMImportCard as c ->
                List.forall (
                    function
                    | Impossible -> false
                    | String str -> c.Data.name.ToLower().Contains(str)
                    | _ -> true
                ) filter
            | _ -> true

module EtternaPacks =

    do
        // EtternaOnline's certificate keeps expiring!! Rop get on it
        // todo: set up automated test that pings eo for certificate expiry
        ServicePointManager.ServerCertificateValidationCallback <-
            RemoteCertificateValidationCallback(
                fun _ cert _ sslPolicyErrors ->
                    if sslPolicyErrors = SslPolicyErrors.None then true
                    else
                        let cert_string = cert.GetCertHashString().ToLower()
                        Logging.Debug(sprintf "Expired certificate: %s (expired on %s)" cert_string (cert.GetExpirationDateString()))
                        cert_string = "5f9a90b88ae54db56d6fcff44ee5e0fb787801ad"
            )

    let tab = 
        let searchContainer =
            SearchContainer(
                (fun searchContainer callback -> 
                    WebServices.download_json("https://api.etternaonline.com/v2/packs/",
                        fun data ->
                        match data with
                        | Some (d: {| data: ResizeArray<EOPack> |}) -> sync(fun () -> for p in d.data do searchContainer.Items.Add(SMImportCard (p.id, p.attributes)))
                        | None -> ()
                        callback()
                    )
                ),
                (fun searchContainer filter -> searchContainer.Items.Filter <- SMImportCard.Filter filter),
                Position = Position.TrimBottom(60.0f)
            )
        StaticContainer(NodeType.Switch(K searchContainer))
        |+ searchContainer
        |+ Text(L"imports.disclaimer.etterna", Position = Position.SliceBottom(60.0f))