namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
open Prelude.Skins.Conversions
open Prelude.Skins.Conversions.Osu
open Interlude.UI
open Interlude.Content
open Interlude.Features.Import.Etterna
open Interlude.Features.Import.osu

module FileDrop =
    
    let import_stepmania_noteskin (path: string) : unit =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")
        let timestamp = "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss")

        let existing_id = Skins.list_noteskins() |> Seq.map (fun (id, _, _) -> id) |> Seq.tryFind (fun x -> x.StartsWith id)
        ImportEtternaNoteskinPage(
            path,
            id + timestamp,
            existing_id
        )
            .Show()
            
    let import_osu_skin (path: string) : unit =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")
        let timestamp = "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss")

        match OsuSkinConverter.check_before_convert path with
        | Ok ini ->
            let existing_id = Skins.list_noteskins() |> Seq.map (fun (id, _, _) -> id) |> Seq.tryFind (fun x -> x.StartsWith id)
            ImportOsuNoteskinPage(
                ini,
                path,
                id + timestamp,
                existing_id
            )
                .Show()
        | Error err ->
            Logging.Error "Error while parsing osu! skin.ini: %O" err
            Notifications.error(%"notification.skin_ini_parse_failed.title", %"notification.skin_ini_parse_failed.body")

    let handle (path: string) : unit =
        assert GameThread.is_game_thread()

        match path with
        | OsuSkinFolder ->
            Menu.Exit()
            import_osu_skin(path)

        | StepmaniaNoteskinFolder ->
            Menu.Exit()
            import_stepmania_noteskin(path)

        | InterludeSkinArchive ->
            try
                File.Copy(path, Path.Combine(get_game_folder "Skins", Path.GetFileName path))
                Skins.load()
            with err ->
                Logging.Error "Error moving/importing dropped skin: %O" err

        | OsuSkinArchive ->
            let id = Path.GetFileNameWithoutExtension(path)
            let target = Path.Combine(get_game_folder "Downloads", id)
            try
                Directory.Delete(target, true)
                ZipFile.ExtractToDirectory(path, target)
            with _ -> ()
            Menu.Exit()
            import_osu_skin target
            // todo: clean up extracted noteskin in downloads

        | _ when Path.GetExtension(path).ToLower() = ".osr" ->
            match OsuReplay.TryReadFile path with
            | Some replay ->
                if Screen.current_type = ScreenType.LevelSelect || Screen.current_type = ScreenType.MainMenu then
                    Replay.figure_out_replay replay
                else
                    Notifications.error(%"osu_replay_import.failed", %"osu_replay_import.wrong_menu")
            | None -> Notifications.error (%"notification.import_failed", "")

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                Menu.Exit()
                ConfirmUnlinkedImportPage(path).Show()
            else

            let task_tracking = TaskTracking.add (Path.GetFileName path)
            let task = Imports.auto_detect_import(path, Content.Library, task_tracking.set_Progress)
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