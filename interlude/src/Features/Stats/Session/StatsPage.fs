namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type StatsPage() =
    inherit Page()

    override this.Content() =
        Container(NodeType.Leaf)
        |+ ActivityFeed(Setting.simple (Timestamp.now() |> timestamp_to_local_day |> DateOnly.FromDateTime),
            Position = Position.SliceLPercent(0.4f).ShrinkT(150.0f).SliceT(200.0f).ShrinkX(40.0f))
        :> Widget

    override this.Title = %"menu.stats"
    override this.OnClose() = ()