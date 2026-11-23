namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Data.Imports
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

module Mounts =

    let init () =

        let import_mount_if_auto_enabled
            (mount: Setting<MountedChartSource option>, name: string, moved_warning: string)
            =
            match mount.Value with
            | Some mount ->
                if Directory.Exists(mount.SourceFolder) then
                    if mount.ImportOnStartup then
                        Logging.Info "Checking for new %s songs to import.." name

                        mount
                            .ImportNewCharts(Content.Library)
                            .Track(TaskContext(sprintf "%s: %s" name %"mount.importall"))
                            .WithCallback(fun (_: Result<_, _>) -> ())
                            .Queue(import_queue)
                else
                    Logging.Warn
                        "%s Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                        name

                    Notifications.Error(moved_warning, %"notification.mount_moved.body")
            | None -> ()

        import_mount_if_auto_enabled(options.OsuMount, "osu!", %"notification.mount_moved.osu")
        import_mount_if_auto_enabled(options.QuaverMount, "Quaver", %"notification.mount_moved.quaver")
        import_mount_if_auto_enabled(options.EtternaMount, "Etterna", %"notification.mount_moved.etterna")
        import_mount_if_auto_enabled(options.StepmaniaMount, "Stepmania", %"notification.mount_moved.stepmania")

    let detect_or_drop_folder (game: MountedGameType) (callback: string -> unit) =
        let default_location = game.DefaultSongsLocation

        if Directory.Exists(default_location) then
            callback(default_location)
        else
            MountFolderPickerPage(game, callback).Show()

    let import_osu_scores (osu_mount: MountedChartSource, notify_start: bool) : unit =

        ImportOsuScoresTask(Path.GetDirectoryName(osu_mount.SourceFolder), Content.Library)
            .Track(BackgroundTaskTracking.add %"mount.import_osu_scores")
            .WithCallback(
                function
                | Ok result ->
                    Notifications.Task(
                        Icons.FOLDER_PLUS,
                        %"notification.score_import_success",
                        [ result.NewScores.ToString(); result.Maps.ToString() ]
                        %> "notification.score_import_success.body"
                    )
                | Error _ -> ()
            )
            .Queue(general_task_queue)

        if notify_start then
            Notifications.System(Icons.ALERT_OCTAGON, %"notification.score_import_queued")
