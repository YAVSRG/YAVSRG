namespace Interlude.Features.Stats

open Percyqaz.Flux.UI
open Prelude.Data.User

type OverallTab() =
    inherit Container(NodeType.Leaf)

    let content_panel = SwapContainer(Skills(), Position = Position.SliceRPercent(0.6f).ShrinkB(80.0f).ShrinkT(200.0f).ShrinkX(40.0f))

    override this.Init(parent) =
        this
        |+ OverallTime(
            (fun () -> Stats.TOTAL_STATS.GameTime + Stats.CURRENT_SESSION.GameTime),
            (fun () -> Stats.TOTAL_STATS.PlayTime + Stats.CURRENT_SESSION.PlayTime),
            (fun () -> Stats.TOTAL_STATS.PracticeTime + Stats.CURRENT_SESSION.PracticeTime),
            Position = Position.SliceLPercent(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)
        )
        |+ PlayCount(
            (fun () -> Stats.TOTAL_STATS.PlaysStarted + Stats.CURRENT_SESSION.PlaysStarted),
            (fun () -> Stats.TOTAL_STATS.PlaysCompleted + Stats.CURRENT_SESSION.PlaysCompleted),
            (fun () -> Stats.TOTAL_STATS.PlaysRetried + Stats.CURRENT_SESSION.PlaysRetried),
            (fun () -> Stats.TOTAL_STATS.PlaysQuit + Stats.CURRENT_SESSION.PlaysQuit),
            Position = Position.SliceLPercent(0.4f).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)
        )
        |* content_panel

        base.Init parent