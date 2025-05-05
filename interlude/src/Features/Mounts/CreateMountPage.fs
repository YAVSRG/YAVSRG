namespace Interlude.Features.Mounts

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data.Library.Imports
open Interlude.UI
open Interlude.Content
open Interlude.Features.Import

type private CreateMountPage(game: MountedGameType, path: string, setting: Setting<MountedChartSource option>) =
    inherit Page()

    let copy_assets = Setting.simple false
    let import_on_startup = Setting.simple false
    let score_import_available = game.IsOsu
    let also_import_scores = Setting.simple false

    let info =
        Callout.Small
            .Title(%"mount.linking_hint.title")
            .Body([game.ToString()] %> "mount.linking_hint.linking")
            .Body([game.ToString()] %> "mount.linking_hint.copying")

    let confirm () =
        let source_type =
            match game with
            | MountedGameType.Osu -> Pack "osu!"
            | MountedGameType.Quaver -> Pack "Quaver"
            | MountedGameType.Etterna -> Library
            | MountedGameType.Stepmania -> Library
        let mount = MountedChartSource.Create(source_type, path, import_on_startup.Value, copy_assets.Value)

        setting.Value <- Some mount

        let task_tracking = sprintf "%O: %s" game %"mount.importall" |> TaskTracking.add
        let task = Mount.import_all(setting.Value.Value, Content.Charts, Content.UserData, task_tracking.set_Progress)
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
                if also_import_scores.Value then GameThread.defer (fun () -> Mounts.import_osu_scores(mount, false))
            | Error reason ->
                Logging.Error "Error importing %s: %s" path reason
                Notifications.error (%"notification.import_failed", reason)
        )

        Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")

        Menu.Back()

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"mount.link_type",
                    Selector(
                        [|
                            false, %"mount.link_type.link_assets"
                            true, %"mount.link_type.copy_assets"
                        |],
                        copy_assets
                    )
                )
                    .Pos(4),
                PageSetting(%"mount.importatstartup",
                    Checkbox(import_on_startup)
                )
                    .Help(Help.Info("mount.importatstartup"))
                    .Pos(6)
            )
            .WithConditional(
                score_import_available,
                PageSetting(%"mount.also_import_scores", Checkbox(also_import_scores)).Pos(8)
            )
            .With(
                PageButton(%"mount.create", confirm)
                    .Icon(Icons.CHECK)
                    .Pos(21),
                Text([game.ToString()] %> "mount.create.title")
                    .Align(Alignment.LEFT)
                    .TextPos(0),
                Text(sprintf "%s: %s" %"mount.create.path" path)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(2),
                CalloutCard(info)
                    .Pos(if score_import_available then 11 else 9)
            )

    override this.Title = %"mount.create"