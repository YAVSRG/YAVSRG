namespace Interlude.Features.Import

open System.Net
open System.Net.Security
open System.IO
open Percyqaz.Json
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data
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

type private SMImportCard(data: EOPackAttrs) as this =
    inherit Frame(NodeType.None,
        Fill = (fun () -> Style.color(120, (if this.Downloaded then 0.7f else 0.5f), 0.0f)),
        Border = (fun () -> Style.color(200, 0.7f, 0.2f))
    )

    let mutable downloaded = 
        let path = Path.Combine(getDataPath "Songs", data.name)
        Directory.Exists path && not ( Seq.isEmpty (Directory.EnumerateDirectories path) )

    let download() =
        let target = Path.Combine(getDataPath "Downloads", System.Guid.NewGuid().ToString() + ".zip")
        Notifications.add (Localisation.localiseWith [data.name] "notification.download.pack", NotificationType.Task)
        // todo: visualise download progress
        WebServices.download_file.Request((data.download, target),
            fun completed ->
                if completed then Library.Imports.auto_convert.Request(target,
                    fun b ->
                        LevelSelect.refresh <- LevelSelect.refresh || b
                        Notifications.add (Localisation.localiseWith [data.name] "notification.install.pack", NotificationType.Task)
                        File.Delete target
                )
            )
        downloaded <- true
    do
        this
        |+ Text(data.name,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f })
        |+ Text(
            (sprintf "%.1fMB" (float data.size / 1000000.0)),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 5.0f; Bottom = 1.0f %- 30.0f })
        |+ Text(
            (fun () -> if downloaded then "Downloaded" else ""),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 5.0f; Bottom = Position.max })
        |+ Text(
            (sprintf "Average difficulty (MSD): %.2f" data.average),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 400.0f; Bottom = Position.max })
        |* Clickable((fun () -> if not downloaded then download()))

    member this.Downloaded = downloaded

    member this.Data = data

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
                    else cert.GetCertHashString().ToLower() = "e87a496fbc4b7914674f3bc3846368234e50fb74" )

    let tab = 
        StaticContainer(NodeType.None)
        |+ SearchContainer(
                (fun searchContainer callback -> 
                    WebServices.download_json("https://api.etternaonline.com/v2/packs/",
                        fun data ->
                        match data with
                        | Some (d: {| data: ResizeArray<EOPack> |}) -> sync(fun () -> for p in d.data do searchContainer.Items.Add(SMImportCard p.attributes))
                        | None -> ()
                        callback()
                    )
                ),
                (fun searchContainer filter -> searchContainer.Items.Filter <- SMImportCard.Filter filter),
                Position = Position.TrimBottom(60.0f)
            )
        |+ Text("(Interlude is not affiliated with Etterna, these downloads are provided through their API)", Position = Position.SliceBottom(60.0f))