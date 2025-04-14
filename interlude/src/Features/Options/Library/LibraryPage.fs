namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Prelude.Data.Maintenance
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

    let vacuum () : unit =
        let task_tracking = TaskTracking.add %"library.vacuum"
        let task = Vacuum.vacuum_charts(true, Content.Charts, task_tracking.set_Progress)
        general_task_queue.Request(task, fun () ->
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.vacuum_complete.title",
                ""
            )
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

    static member LevelSelectOptions() : PageButton =
        PageButton(%"levelselect.options", fun () -> LevelSelectOptionsPage().Show())

    static member Vacuum() : PageButton =
        PageButton.Once(%"library.vacuum", LibraryActions.vacuum)
            .Help(Help.Info("library.vacuum"))

    override this.Content() =

        let main_options =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .Position({ Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y) with Right = 0.5f %- 10.0f })
                .With(
                    LibraryPage.Imports().Pos(0, 2, PageWidth.Full),
                    LibraryPage.ManageCollections().Pos(2, 2, PageWidth.Full),
                    LibraryPage.ManageTables().Pos(4, 2, PageWidth.Full),
                    LibraryPage.ManageRulesets().Pos(6, 2, PageWidth.Full),
                    LibraryPage.LevelSelectOptions().Pos(9, 2, PageWidth.Full),
                    LibraryPage.RecachePatterns().Pos(11, 2, PageWidth.Full),
                    LibraryPage.RecalculateScores().Pos(13, 2, PageWidth.Full),
                    LibraryPage.Vacuum().Pos(15, 2, PageWidth.Full),
                    Text([Content.Library.Charts.Entries.Count.ToString()] %> "library.chart_count")
                        .Align(Alignment.LEFT)
                        .TextPos(21),
                    Text(%"library.import_in_progress_hint")
                        .Color(Colors.text_green)
                        .Align(Alignment.LEFT)
                        .Conditional(TaskTracking.in_progress)
                        .TextPosSmall(20)
                )

        NavigationContainer.Row()
            .With(
                main_options
                    .Position({ Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y) with Right = 0.5f %- 10.0f }),

                MountDisplay.CreateAll()
                    .Position(Position.Shrink(PAGE_MARGIN_X).SliceR(MountDisplay.WIDTH).SliceY(MountDisplay.ALL_HEIGHT))
            )

    override this.Title = %"library"
    override this.OnClose() = ()