namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.UserDatabase
open Interlude.UI
open Interlude.Content

type AllYearsActivityPage(selected: Setting<(DateOnly * Session) option>) =
    inherit Page()

    override this.Content() : Widget =
        let flow = FlowContainer.Vertical<YearActivityGrid>(200.0f, Spacing = 65.0f)

        for year in Content.Stats.GetPreviousSessions() |> Seq.map _.Key.Year |> Seq.distinct |> Seq.sortDescending do
            flow.Add(YearActivityGrid(year, selected |> Setting.trigger(fun _ -> Menu.Back())))

        ScrollContainer(flow).Margin(50.0f).Position(Position.ShrinkT(120.0f).ShrinkB(80.0f).SliceX(1380.0f))

    override this.Title = %"stats.activity"
