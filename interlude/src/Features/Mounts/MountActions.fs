namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Formats.Conversions
open Prelude.Data.Imports
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

[<RequireQualifiedAccess>]
type MountedGameType =
    | Osu
    | Quaver
    | Etterna
    | Stepmania

    override this.ToString() : string =
        match this with
        | Osu -> "osu!mania"
        | Quaver -> "Quaver"
        | Etterna -> "Etterna"
        | Stepmania -> "Stepmania"

    member this.Color =
        match this with
        | Osu -> Color.FromArgb(0xFF_FF79B8)
        | Quaver -> Color.FromArgb(0xFF_39B6D1)
        | Etterna -> Color.FromArgb(0xFF_8F60F6)
        | Stepmania -> Color.FromArgb(0xFF_FF703C)

    member this.DefaultSongsLocation: string =
        match this with
        | MountedGameType.Osu -> MountedChartSource.DEFAULT_OSU_SONGS_LOCATION
        | MountedGameType.Quaver -> MountedChartSource.DEFAULT_QUAVER_SONGS_LOCATION
        | MountedGameType.Stepmania -> MountedChartSource.DEFAULT_STEPMANIA_SONGS_LOCATION
        | MountedGameType.Etterna -> MountedChartSource.DEFAULT_ETTERNA_SONGS_LOCATION

type private MountFolderPickerPage(game: MountedGameType, callback: string -> unit) =
    inherit Page()

    let info =
        match game with
        | MountedGameType.Osu ->
            Callout.Normal.Icon(Icons.FOLDER_PLUS).Title(%"mount.create.osu.prompt").Body(%"mount.create.folder_hint")
        | MountedGameType.Quaver ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.quaver.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Stepmania ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.stepmania.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Etterna ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.etterna.prompt")
                .Body(%"mount.create.folder_hint")

    override this.Content() : Widget =
        let path_chosen (path: string) : unit =
            match game, path with
            | MountedGameType.Osu, PackFolder
            | MountedGameType.Quaver, PackFolder
            | MountedGameType.Stepmania, FolderOfPacks
            | MountedGameType.Etterna, FolderOfPacks ->
                Menu.Back()
                callback path

            | MountedGameType.Osu, _ -> Notifications.error(%"mount.create.osu.error", "")
            | MountedGameType.Quaver, _ -> Notifications.error(%"mount.create.quaver.error", "")
            | MountedGameType.Stepmania, _ -> Notifications.error(%"mount.create.stepmania.error", "")
            | MountedGameType.Etterna, _ -> Notifications.error(%"mount.create.etterna.error", "")

        let choose_path () : unit =
            FileDialog.pick_folder(
                null,
                function
                | FileDialogResult.Ok path -> path_chosen path
                | FileDialogResult.Cancelled -> ()
                | FileDialogResult.Error -> Notifications.error(%"notification.file_picker_error", "")
            )

        page_container().With(PageButton(%"mount.choose_folder", choose_path).Pos(6), CalloutCard(info).Pos(0))

    override this.Title = %"mount.folder_not_detected"

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
                            .Track(TaskTracking.add(sprintf "%s: %s" name %"mount.importall")) // create tracking that isn't listed
                            .WithCallback(
                                function
                                | Ok _ -> ()
                                | Error _ -> ()
                            )
                            .Queue(import_queue)
                else
                    Logging.Warn
                        "%s Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                        name

                    Notifications.error(moved_warning, %"notification.mount_moved.body")
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
            .Track(TaskTracking.add %"mount.import_osu_scores")
            .WithCallback(
                function
                | Ok result ->
                    Notifications.task_feedback(
                        Icons.FOLDER_PLUS,
                        %"notification.score_import_success",
                        [ result.NewScores.ToString(); result.Maps.ToString() ]
                        %> "notification.score_import_success.body"
                    )
                | Error _ -> ()
            )
            .Queue(general_task_queue)

        if notify_start then
            Notifications.system_feedback(Icons.ALERT_OCTAGON, %"notification.score_import_queued", "")
