namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
open Prelude.Skins.Conversions
open Interlude.UI
open Interlude.Content

module FileDrop =

    let handle (path: string) : unit =
        assert GameThread.is_game_thread()

        match path with
        | OsuSkinFolder ->
            Menu.Exit()
            osu.Skins.import_osu_skin path

        | StepmaniaNoteskinFolder ->
            Menu.Exit()
            Etterna.Skins.import_stepmania_noteskin path

        | InterludeSkinArchive ->
            try
                File.Copy(path, Path.Combine(get_game_folder "Skins", Path.GetFileName path))
                Skins.load()
            with err ->
                Logging.Error "Error moving/importing dropped skin: %O" err

        | OsuSkinArchive ->
            let id = Path.GetFileNameWithoutExtension(path)
            let target = Path.Combine(get_game_folder "Downloads", id)
            try Directory.Delete(target, true) with _ -> ()
            ZipFile.ExtractToDirectory(path, target)
            Menu.Exit()
            osu.Skins.import_osu_skin target
            // todo: clean up extracted noteskin in downloads

        | _ when Path.GetExtension(path).ToLower() = ".osr" ->
            match OsuReplay.TryReadFile path with
            | Some replay ->
                if Screen.current_type = ScreenType.LevelSelect || Screen.current_type = ScreenType.MainMenu then
                    osu.Replay.figure_out_replay replay
                else
                    Notifications.error(%"osu_replay_import.failed", %"osu_replay_import.wrong_menu")
            | None -> Notifications.error (%"notification.import_failed", "")

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                Menu.Exit()
                ConfirmUnlinkedImportPage(path).Show()
            else

            let task_tracking = TaskTracking.add (Path.GetFileName path)
            let task = Imports.auto_detect_import(path, Content.Charts, Content.UserData, task_tracking.set_Progress)
            import_queue.Request(task,
                function
                | Ok result ->
                    Notifications.task_feedback (
                        Icons.CHECK,
                        %"notification.import_success",
                        [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                    )
                    Content.TriggerChartAdded()
                | Error reason ->
                    Logging.Error "Error importing %s: %s" path reason
                    Notifications.error (%"notification.import_failed", reason)
            )