namespace Interlude.Features.Import

open Percyqaz.Common
open Prelude
open Prelude.Data.Library.Caching
open Interlude.Content
open Interlude.Web.Shared.Requests

[<AutoOpen>]
module Import =

    let mutable on_file_drop : (string -> unit) option = None

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish

    let download_chart_by_hash =
        { new Async.Service<string * string, bool>() with
            override _.Handle((chart_id, folder_name)) =
                async {
                    match Cache.by_hash chart_id Content.Cache with
                    | Some cc -> return true
                    | None ->

                    let mutable identified_chart : Charts.Identify.Response option = None
                    do! Charts.Identify.get_async(chart_id, fun _res -> identified_chart <- _res)

                    match identified_chart with
                    | None -> return false
                    | Some server_response ->

                    match server_response.Info with
                    | None -> return false
                    | Some found ->

                    return! Cache.cdn_download folder_name chart_id (found.Chart, found.Song) Content.Cache
                }
        }

type private DownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed