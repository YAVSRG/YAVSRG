namespace Interlude.Content

open Percyqaz.Common
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Interlude.Web.Shared.Requests

module Backbeat =

    // todo: move as much into prelude as possible
    let download_missing_chart =
        { new Async.Queue<string * string, bool>() with
            override _.Handle((chart_id, folder_name)) =
                async {
                    match ChartDatabase.get_meta chart_id Content.Charts with
                    | Some chart_meta -> return true
                    | None ->

                    let mutable identified_chart : Charts.Identify.Response option = None
                    do! Charts.Identify.get_async(chart_id, fun _res -> identified_chart <- _res)

                    match identified_chart with
                    | None -> return false
                    | Some server_response ->

                    match server_response.Info with
                    | None -> return false
                    | Some found ->

                    match! OnlineImports.cdn_install (folder_name, chart_id, found.Chart, found.Song, Content.Charts) with
                    | Ok () ->
                        Content.TriggerChartAdded()
                        return true
                    | Error reason ->
                        Logging.Error "Error downloading '%s' from CDN: %s" chart_id reason
                        return false
                }
        }