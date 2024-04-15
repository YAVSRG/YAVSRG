namespace Interlude.Features.Import

open Percyqaz.Common
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type private EditMountPage(setting: Setting<Imports.MountedChartSource option>) as this =
    inherit Page()

    let mount = setting.Value.Value
    let import_on_startup = Setting.simple mount.ImportOnStartup
    let mutable import = false

    do
        page_container()
        |+ PageSetting("mount.importatstartup", Selector<_>.FromBool import_on_startup)
            .Pos(0)
            .Tooltip(Tooltip.Info("mount.importatstartup"))
        |+ PageButton
            .Once(
                "mount.import",
                fun () ->
                    import <- true
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
            )
            .Pos(3)
            .Tooltip(Tooltip.Info("mount.import"))
        |+ PageButton
            .Once(
                "mount.importall",
                fun () ->
                    import <- true
                    mount.LastImported <- System.DateTime.UnixEpoch
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
            )
            .Pos(5)
            .Tooltip(Tooltip.Info("mount.importall"))
        |> fun col ->
            if
                obj.ReferenceEquals(setting, options.OsuMount)
                && mount.LastImported <> System.DateTime.UnixEpoch
            then
                col
                |+ PageButton
                    .Once(
                        "mount.import_osu_scores",
                        fun () ->
                            Scores.import_osu_scores_service.Request(
                                (),
                                fun () ->
                                    Notifications.task_feedback (
                                        Icons.FOLDER_PLUS,
                                        %"notification.score_import_success",
                                        ""
                                    )
                            )

                            Notifications.action_feedback (
                                Icons.FOLDER_PLUS,
                                %"notification.score_import_queued",
                                ""
                            )
                    )
                    .Pos(8)
                    .Tooltip(Tooltip.Info("mount.import_osu_scores"))
            else
                col
        |> this.Content

    override this.Title = %"mount.name"

    override this.OnClose() =
        setting.Value <-
            Some
                { mount with
                    ImportOnStartup = import_on_startup.Value
                }

        if import then
            Imports.import_mounted_source.Request(
                (setting.Value.Value, Content.Library),
                fun () -> Notifications.task_feedback (Icons.FOLDER_PLUS, %"notification.import_success", "")
            )