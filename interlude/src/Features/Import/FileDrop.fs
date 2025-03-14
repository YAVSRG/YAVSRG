﻿namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
open Prelude.Skins.Conversions
open Interlude.UI
open Interlude.Content

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
                fun () -> Menu.Back() // todo: open the library options menu
            )
            .Pos(8)
        |+ PageButton
            .Once(
                %"unlinkedsongsimport.confirm",
                fun () ->
                    Imports.auto_convert.Request(
                        (path, false, Content.Charts, Content.UserData),
                        function
                        | Some result ->
                            Notifications.task_feedback (
                                Icons.CHECK,
                                %"notification.import_success",
                                [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                            )
                            Content.TriggerChartAdded()
                        | None ->
                            Notifications.error (%"notification.import_failed", "")
                    )

                    Menu.Back()
            )
            .Pos(11)
        |+ Callout.frame info (fun (w, h) -> Position.Box(0.0f, 0.0f, 0.0f, -10.0f, w, h))
        :> Widget

    override this.Title = %"unlinkedsongsimport"
    override this.OnClose() = ()

module FileDrop =

    let mutable on_file_drop : (string -> unit) option = None

    let handle (path: string) =
        assert(GameThread.is_game_thread())

        match on_file_drop with
        | Some f -> f path
        | None ->

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
                if Screen.current_type = Screen.Type.LevelSelect || Screen.current_type = Screen.Type.MainMenu then
                    osu.Replay.figure_out_replay replay
                else
                    Notifications.error("Replay import failed!", "Must be on level select or main menu screen")
            | None -> Notifications.error (%"notification.import_failed", "")

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                Menu.Exit()
                ConfirmUnlinkedSongsImport(path).Show()
            else

            Imports.auto_convert.Request(
                (path, false, Content.Charts, Content.UserData),
                function
                | Some result ->
                    Notifications.task_feedback (
                        Icons.CHECK,
                        %"notification.import_success",
                        [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                    )
                    Content.TriggerChartAdded()
                | None ->
                    Notifications.error (%"notification.import_failed", "")
            )