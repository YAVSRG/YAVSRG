namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library
open Prelude.Skinning.Noteskins.Conversion
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Web.Shared.Requests

[<AutoOpen>]
module Import =

    let mutable on_file_drop : (string -> unit) option = None

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish

    type ConfirmUnlinkedSongsImport(path) =
        inherit Page()

        let info =
            Callout.Normal
                .Icon(Icons.ALERT_CIRCLE)
                .Title(%"unlinkedsongsimport.info.title")
                .Body(%"unlinkedsongsimport.info.body")

        override this.Content() =
            page_container()
            |+ PageButton
                .Once(
                    %"unlinkedsongsimport.link_intended",
                    fun () ->
                        Screen.change Screen.Type.Import Transitions.Default |> ignore
                        Menu.Back()
                )
                .Pos(6)
            |+ PageButton
                .Once(
                    %"unlinkedsongsimport.confirm",
                    fun () ->
                        Imports.auto_convert.Request(
                            (path, false, Content.Library),
                            function
                            | Some result ->
                                Notifications.task_feedback (
                                    Icons.CHECK, 
                                    %"notification.import_success",
                                    [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                                )
                                defer charts_updated_ev.Trigger
                            | None ->
                                Notifications.error (%"notification.import_failed", "")
                        )

                        Menu.Back()
                )
                .Pos(9)
            |+ Callout.frame info (fun (w, h) -> Position.Box(0.0f, 0.0f, 100.0f, 200.0f, w, h))
            :> Widget

        override this.Title = %"unlinkedsongsimport"
        override this.OnClose() = ()

    let handle_file_drop (path: string) =
        match on_file_drop with
        | Some f -> f path
        | None ->

        match path with
        | OsuSkinFolder -> 
            defer 
            <| fun () -> 
                Menu.Exit()
                FromOsu.ImportSkins.import_osu_noteskin path

        | InterludeSkinArchive ->
            try
                File.Copy(path, Path.Combine(get_game_folder "Noteskins", Path.GetFileName path))
                Noteskins.load ()
            with err ->
                Logging.Error("Something went wrong when moving this skin!", err)

        | OsuSkinArchive ->
            let id = Path.GetFileNameWithoutExtension(path)
            let target = Path.Combine(get_game_folder "Downloads", id)
            ZipFile.ExtractToDirectory(path, target)
            defer 
            <| fun () -> 
                Menu.Exit()
                FromOsu.ImportSkins.import_osu_noteskin target
            // todo: clean up extracted noteskin in downloads

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                defer
                <| fun () ->
                    Menu.Exit()
                    ConfirmUnlinkedSongsImport(path).Show()
            else

            Imports.auto_convert.Request(
                (path, false, Content.Library),
                function
                | Some result ->
                    Notifications.task_feedback (
                        Icons.CHECK, 
                        %"notification.import_success",
                        [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                    )
                    defer charts_updated_ev.Trigger
                | None ->
                    Notifications.error (%"notification.import_failed", "")
            )

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

[<RequireQualifiedAccess>]
type private MountedGameType =
    | Osu
    | Quaver
    | Etterna
    | Stepmania