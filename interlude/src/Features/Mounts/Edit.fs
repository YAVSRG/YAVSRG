namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Data.Library.Imports
open Prelude.Data.OsuClientInterop
open Percyqaz.Flux.UI
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

type private EditMountPage(game: MountedGameType, setting: Setting<MountedChartSource option>) =
    inherit Page()

    let mount = setting.Value.Value
    let import_on_startup = Setting.simple mount.ImportOnStartup
    let mutable import = false

    let import_osu_scores () : unit =

        let task_tracking = TaskTracking.add %"mount.import_osu_scores"
        let task =
            let import_task = Scores.import_osu_scores_async(Path.GetDirectoryName mount.SourceFolder, Content.Charts, Content.UserData, task_tracking.set_Progress)
            async {
                let! result = import_task
                Notifications.task_feedback (
                    Icons.FOLDER_PLUS,
                    %"notification.score_import_success",
                    [ result.NewScores.ToString(); result.Maps.ToString() ] %> "notification.score_import_success.body"
                )
            }
        general_task_queue.Request(task, ignore)

        Notifications.system_feedback (
            Icons.ALERT_OCTAGON,
            %"notification.score_import_queued",
            ""
        )

    override this.Content() =
        page_container()
        |+ PageSetting(%"mount.importatstartup", Checkbox import_on_startup)
            .Help(Help.Info("mount.importatstartup"))
            .Pos(0)
        |+ PageButton
            .Once(
                %"mount.import",
                fun () ->
                    import <- true
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
            )
            .Help(Help.Info("mount.import"))
            .Pos(3)
        |+ PageButton
            .Once(
                %"mount.importall",
                fun () ->
                    import <- true
                    mount.LastImported <- None
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
            )
            .Help(Help.Info("mount.importall"))
            .Pos(5)
        |+
            if
                game = MountedGameType.Osu
                && mount.LastImported.IsSome
            then
                PageButton.Once(
                    %"mount.import_osu_scores",
                    import_osu_scores
                )
                    .Help(Help.Info("mount.import_osu_scores"))
                    .Pos(8)
                    :> Widget
            else
                Dummy()
        |+
            if
                game = MountedGameType.Osu
            then
                PageButton(
                    %"mount.import_osu_skins",
                    fun () -> osu.Skins.OsuSkinsListPage().Show()
                )
                    .Help(Help.Info("mount.import_osu_skins"))
                    .Pos(10)
                    :> Widget
            else
                Dummy()
        :> Widget

    override this.Title = %"mount"

    override this.OnClose() =
        setting.Value <-
            Some
                { mount with
                    ImportOnStartup = import_on_startup.Value
                }

        if import then
            let task_tracking =
                match setting.Value.Value.LastImported with
                | Some _ -> %"mount.import"
                | None -> %"mount.importall"
                |> sprintf "%O: %s" game
                |> TaskTracking.add
            let task = Mount.import_new(setting.Value.Value, Content.Charts, Content.UserData, task_tracking.set_Progress)
            import_queue.Request(task,
                function
                | Ok result ->
                    Notifications.task_feedback (
                        Icons.CHECK,
                        %"notification.import_success",
                        [ result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString() ]
                        %> "notification.import_success.body"
                    )
                    Content.TriggerChartAdded()
                | Error reason ->
                    Logging.Error "Error importing %s: %s" setting.Value.Value.SourceFolder reason
                    Notifications.error (%"notification.import_failed", reason)
            )

            Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")