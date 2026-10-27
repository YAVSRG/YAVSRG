namespace Interlude.Features.Mounts

open Percyqaz.Common
open Prelude
open Prelude.Data
open Prelude.Data.Imports
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

type private EditMountPage
    (game: MountedGameType, mount: MountedChartSource, setting: Setting<MountedChartSource option>) =
    inherit Page()

    let copy_assets = Setting.simple mount.CopyAssetFiles
    let import_on_startup = Setting.simple mount.ImportOnStartup
    let mutable queued_import = 0

    let linked_chart_count = mount.FindLinkedCharts(Content.Charts) |> Seq.length

    let delete () : unit =
        setting.Value <- None
        queued_import <- -1
        mount.DeleteLinkedCharts(Content.Charts)
        Content.TriggerChartAdded(None)
        Notifications.action_feedback(Icons.TRASH, %"notification.mount_deleted", linked_chart_count.ToString())
        Menu.Back()

    member this.SaveChangesAndImport() : unit =

        if queued_import <> -1 then
            setting.Value <-
                Some { mount with ImportOnStartup = import_on_startup.Value; CopyAssetFiles = copy_assets.Value }

        if queued_import = 1 then
            setting.Value.Value
                .ImportNewCharts(Content.Library)
                .Track(TaskTracking.add(sprintf "%O: %s" game %"mount.import"))
                .WithCallback(FileDrop.conversion_result_callback)
                .Queue(import_queue)

        elif queued_import = 2 then
            setting.Value.Value
                .ImportAllCharts(Content.Library)
                .Track(TaskTracking.add(sprintf "%O: %s" game %"mount.importall"))
                .WithCallback(FileDrop.conversion_result_callback)
                .Queue(import_queue)

    override this.Content() : Widget =
        this.OnClose(this.SaveChangesAndImport)

        page_container()
            .With(
                PageSetting(%"mount.importatstartup", Checkbox(import_on_startup))
                    .Help(Help.Info("mount.importatstartup"))
                    .Pos(4),
                PageSetting(
                    %"mount.link_type",
                    Selector(
                        [|
                            false, %"mount.link_type.link_assets"
                            true, %"mount.link_type.copy_assets"
                        |],
                        copy_assets
                    )
                )
                    .Pos(6),
                Text(%"mount.link_type.change_to_link_hint")
                    .Color(Colors.text_yellow_2)
                    .Conditional(fun () -> not copy_assets.Value && mount.CopyAssetFiles)
                    .Pos(8, 1),
                PageButton
                    .Once(
                        %"mount.import",
                        fun () ->
                            queued_import <- 1
                            Notifications.action_feedback(Icons.FOLDER_PLUS, %"notification.import_queued", "")
                    )
                    .Help(Help.Info("mount.import"))
                    .Pos(9),
                PageButton
                    .Once(
                        %"mount.importall",
                        fun () ->
                            queued_import <- 2
                            Notifications.action_feedback(Icons.FOLDER_PLUS, %"notification.import_queued", "")
                    )
                    .Help(Help.Info("mount.importall"))
                    .Pos(11),

                PageButton
                    .Once(%"mount.import_osu_scores", fun () -> Mounts.import_osu_scores(mount, true))
                    .Disabled(fun () -> mount.LastImported.IsNone)
                    .Help(Help.Info("mount.import_osu_scores"))
                    .Conditional(K game.IsOsu)
                    .Pos(14),

                PageButton(
                    %"mount.delete" + (if linked_chart_count > 0 then sprintf " (%i)" linked_chart_count else ""),
                    fun () ->
                        if linked_chart_count = 0 then
                            delete()
                        else
                            ConfirmPage(
                                [ linked_chart_count.ToString() ] %> "mount.confirm_delete",
                                fun () -> GameThread.defer delete
                            )
                                .Show()
                )
                    .Disabled(fun () -> queued_import > 0)
                    .TextColor(Colors.red_accent)
                    .Icon(Icons.TRASH)
                    .Pos(21),

                Text([ game.ToString() ] %> "mount.edit.title").Align(Alignment.LEFT).TextPos(0),
                Text(
                    [
                        match mount.LastImported with
                        | Some date -> date.ToString()
                        | None -> "--"
                    ]
                    %> "mount.lastimported"
                )
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(2)
            )

    override this.Title = %"mount"
