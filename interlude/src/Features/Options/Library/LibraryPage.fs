namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Prelude.Data.Maintenance
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Mounts
open Interlude.Features.LevelSelect
open Interlude.Features.Rulesets

module LibraryActions =

    let recalculate_pbs () : unit =
        let rulesets = Rulesets.list() |> Seq.map (fun (id, rs) -> Ruleset.hash rs, rs) |> Array.ofSeq
        let task_tracking = TaskTracking.add %"library.recalculate_personal_bests"
        let task = PersonalBests.recalculate(rulesets, false, Content.Charts, Content.UserData, task_tracking.set_Progress)
        general_task_queue.Request(task, fun () ->
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.score_recalculation_complete.title",
                ""
            )
        )

        Notifications.system_feedback (
            Icons.ALERT_OCTAGON,
            %"notification.score_recalculation_started.title",
            %"notification.score_recalculation_started.body"
        )

    let recalculate_patterns () : unit =
        let task_tracking = TaskTracking.add %"library.recache_patterns"
        let task = Patterns.recalculate(Content.Charts, task_tracking.set_Progress)
        general_task_queue.Request(task, fun () ->
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.pattern_cache_complete.title",
                ""
            )
        )

        Notifications.system_feedback (
            Icons.ALERT_OCTAGON,
            %"notification.pattern_cache_started.title",
            %"notification.pattern_cache_started.body"
        )

type LibraryPage() =
    inherit Page()

    static member ManageCollections() : PageButton =
        PageButton(%"library.collections", fun () -> ManageCollectionsPage().Show())
            .Help(Help.Info("library.collections"))

    static member ManageTables() : PageButton =
        PageButton(%"library.tables", fun () -> SelectTablePage(LevelSelect.refresh_all).Show())
            .Help(Help.Info("library.tables"))

    static member ManageRulesets() : PageButton =
        PageButton(%"rulesets", fun () -> SelectRulesetPage().Show())
            .Help(Help.Info("rulesets"))

    static member RecachePatterns() : PageButton =
        PageButton
            .Once(
                %"library.recache_patterns",
                LibraryActions.recalculate_patterns
            )
            .Help(Help.Info("library.recache_patterns"))

    static member RecalculateScores() : PageButton =
        PageButton
            .Once(
                %"library.recalculate_personal_bests",
                LibraryActions.recalculate_pbs
            )
            .Help(Help.Info("library.recalculate_personal_bests"))

    static member Imports() : PageButton =
        PageButton(%"menu.import", fun () -> ImportsPage().Show())
            .Icon(Icons.DOWNLOAD)

    override this.Content() =
        let import_info =
            Container(NodeType.None)
                .Position(page_position(PAGE_BOTTOM - 4, 4, PageWidth.Custom 300.0f))
            |+ Text(fun () ->
                if TaskTracking.in_progress () then
                    %"imports.in_progress"
                else
                    %"imports.not_in_progress"
            )
                .Color(fun () ->
                    if TaskTracking.in_progress () then
                        Colors.text_green
                    else
                        Colors.text_subheading
                )
                .Position(Position.SliceT(40.0f).Shrink(20.0f, 0.0f))
            |+ LoadingIndicator.Strip(TaskTracking.in_progress)
                .Position(Position.SliceT(40.0f, Style.PADDING).Shrink(150.0f, 0.0f))
            |+ Text(sprintf "%i charts installed" Content.Library.Charts.Entries.Count)
                .Color(Colors.text_subheading)
                .Position(Position.SliceT(65.0f, 30.0f).Shrink(20.0f, 0.0f))

        let main_options =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .Position({ Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y) with Right = 0.5f %- 10.0f })
            |+ LibraryPage.Imports().Pos(0, 2, PageWidth.Full)
            |+ LibraryPage.ManageCollections().Pos(2, 2, PageWidth.Full)
            |+ LibraryPage.ManageTables().Pos(4, 2, PageWidth.Full)
            |+ LibraryPage.RecachePatterns().Pos(7, 2, PageWidth.Full)
            |+ LibraryPage.RecalculateScores().Pos(9, 2, PageWidth.Full)
            |+ PageButton(%"levelselect.options", fun () -> LevelSelectOptionsPage().Show())
                .Pos(12, 2, PageWidth.Full)
            |+ import_info

        let mount_options =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SlicePercentR(0.5f).ShrinkR(10.0f).TranslateY(-50.0f))
            |+ MountControl(MountedGameType.Osu, options.OsuMount)
                .Position(Position.SliceT(100.0f, 150.0f))
            |+ MountControl(MountedGameType.Quaver, options.QuaverMount)
                .Position(Position.SliceT(270.0f, 150.0f))
            |+ MountControl(MountedGameType.Etterna, options.EtternaMount)
                .Position(Position.SliceT(440.0f, 150.0f))
            |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount)
                .Position(Position.SliceT(610.0f, 150.0f))
            |+ Text(%"imports.mount")
                .Align(Alignment.CENTER)
                .Position(Position.SliceT(0.0f, 80.0f))
            |+ Text(%"imports.drag_and_drop_hint")
                .Align(Alignment.CENTER)
                .Position(Position.SliceT(770.0f, 80.0f).Translate(0.0f, -10.0f))

        NavigationContainer.Row()
        |+ main_options
        |+ mount_options
        :> Widget

    override this.Title = %"library"
    override this.OnClose() = ()