namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Library
open Prelude
open Prelude.Content.Noteskins.Conversion
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content

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

        override this.Init(parent) =
            page_container()
            |+ PageButton
                .Once(
                    "unlinkedsongsimport.link_intended",
                    fun () ->
                        Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
                        Menu.Back()
                )
                .Pos(6)
            |+ PageButton
                .Once(
                    "unlinkedsongsimport.confirm",
                    fun () ->
                        Imports.auto_convert.Request(
                            (path, false, Content.Library),
                            fun success ->
                                if success then
                                    Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                                    charts_updated_ev.Trigger()
                                else
                                    Notifications.error (%"notification.import_failure", "")
                        )

                        Menu.Back()
                )
                .Pos(9)
            |+ Callout.frame info (fun (w, h) -> Position.Box(0.0f, 0.0f, 100.0f, 200.0f, w, h))
            |> this.Content
            base.Init parent

        override this.Title = %"unlinkedsongsimport.name"
        override this.OnClose() = ()

    let handle_file_drop (path: string) =
        match on_file_drop with
        | Some f -> f path
        | None ->

        match path with
        | OsuSkinFolder -> 
            sync 
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
            sync 
            <| fun () -> 
                Menu.Exit()
                FromOsu.ImportSkins.import_osu_noteskin target
            // todo: clean up extracted noteskin in downloads

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                sync
                <| fun () ->
                    Menu.Exit()
                    ConfirmUnlinkedSongsImport(path).Show()
            else

            Imports.auto_convert.Request(
                (path, false, Content.Library),
                fun success ->
                    if success then
                        Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                        sync <| fun () -> charts_updated_ev.Trigger()
                    else
                        Notifications.error (%"notification.import_failure", "")
            )

type private DownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

[<RequireQualifiedAccess>]
type private MountedGameType =
    | Osu
    | Stepmania
    | Etterna