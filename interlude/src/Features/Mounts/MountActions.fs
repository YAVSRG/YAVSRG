namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Formats
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
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
    override this.ToString() =
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

type private MountFileDropPage(game: MountedGameType, callback: string -> unit) =
    inherit Page()

    let info =
        match game with
        | MountedGameType.Osu ->
            Callout.Normal
                .Icon(Icons.DOWNLOAD)
                .Title(%"mount.create.osu.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Quaver ->
            Callout.Normal
                .Icon(Icons.DOWNLOAD)
                .Title(%"mount.create.quaver.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Stepmania ->
            Callout.Normal
                .Icon(Icons.DOWNLOAD)
                .Title(%"mount.create.stepmania.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Etterna ->
            Callout.Normal
                .Icon(Icons.DOWNLOAD)
                .Title(%"mount.create.etterna.prompt")
                .Body(%"mount.create.folder_hint")

    override this.Content() =
        FileDrop.on_file_drop <-
            fun path ->
                match game, path with
                | MountedGameType.Osu, PackFolder
                | MountedGameType.Quaver, PackFolder
                | MountedGameType.Stepmania, FolderOfPacks
                | MountedGameType.Etterna, FolderOfPacks -> Menu.Back(); callback path;

                | MountedGameType.Osu, _ -> Notifications.error (%"mount.create.osu.error", "")
                | MountedGameType.Quaver, _ -> Notifications.error (%"mount.create.quaver.error", "")
                | MountedGameType.Stepmania, _ -> Notifications.error (%"mount.create.stepmania.error", "")
                | MountedGameType.Etterna, _ -> Notifications.error (%"mount.create.etterna.error", "")
            |> Some
        this.OnClose(fun () -> FileDrop.on_file_drop <- None)

        page_container()
            .With(
                Dummy(NodeType.Leaf),
                CalloutCard(info).Pos(0)
            )

    override this.Title = %"mount.folder_not_detected"

module Mounts =

    let init () =

        let import_mount_if_auto_enabled(mount: Setting<MountedChartSource option>, name: string, moved_warning: string) =
            match mount.Value with
            | Some mount ->
                if Directory.Exists mount.SourceFolder then
                    if mount.ImportOnStartup then
                        Logging.Info "Checking for new %s songs to import.." name
                        let task = Mount.import_new(mount, Content.Charts, Content.UserData, ignore)
                        import_queue.Request(task, ignore)
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

    let private auto_detect_location (game: MountedGameType) : string =
        match game with
        | MountedGameType.Osu -> OSU_SONG_FOLDER
        | MountedGameType.Quaver -> QUAVER_SONG_FOLDER
        | MountedGameType.Stepmania -> STEPMANIA_PACK_FOLDER
        | MountedGameType.Etterna -> ETTERNA_PACK_FOLDER

    let detect_or_drop_folder (game: MountedGameType) (callback: string -> unit) =
        let location = auto_detect_location game

        if Directory.Exists location then
            callback location
        else
            MountFileDropPage(game, callback).Show()

    let import_osu_scores (osu_mount: MountedChartSource, notify_start: bool) : unit =

        let task_tracking = TaskTracking.add %"mount.import_osu_scores"
        let task =
            let import_task = Scores.import_osu_scores_async(Path.GetDirectoryName osu_mount.SourceFolder, Content.Charts, Content.UserData, task_tracking.set_Progress)
            async {
                let! result = import_task
                Notifications.task_feedback (
                    Icons.FOLDER_PLUS,
                    %"notification.score_import_success",
                    [ result.NewScores.ToString(); result.Maps.ToString() ] %> "notification.score_import_success.body"
                )
            }
        general_task_queue.Request(task, ignore)

        if notify_start then
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.score_import_queued",
                ""
            )