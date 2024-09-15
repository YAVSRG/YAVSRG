namespace Interlude.Features.Import

open Percyqaz.Common
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.Web.Shared.Requests

module Backbeat =

    let download_missing_chart =
        { new Async.Service<string * string, bool>() with
            override _.Handle((chart_id, folder_name)) =
                async {
                    match ChartDatabase.get_meta chart_id Content.Charts with
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

                    let! success = ChartDatabase.cdn_download folder_name chart_id (found.Chart, found.Song) Content.Charts
                    if success then Content.TriggerChartAdded()
                    return success
                }
        }