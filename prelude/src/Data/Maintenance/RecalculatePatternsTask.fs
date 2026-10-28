namespace Prelude.Data.Maintenance

open Percyqaz.Common
open Prelude
open Prelude.Calculator
open Prelude.Calculator.Patterns
open Prelude.Data.Imports
open Prelude.Data.ChartDatabase

type RecalculatePatternsTask(chart_db: ChartDatabase) =
    inherit BackgroundTask<unit>()

    override this.Run(ctx: ITaskContext) : Async<unit> =
        chart_db.RecalculationNeeded <- false

        async {
            seq {
                let charts = chart_db.Entries |> Seq.toArray

                for i, entry in Seq.indexed charts do
                    match chart_db.GetChart(entry.Hash) with
                    | Ok chart ->
                        let difficulty = Difficulty.calculate(1.0f<rate>, chart.ToNoteData())
                        yield entry.Hash, difficulty.Overall, PatternReport.from_chart(difficulty, chart)
                    | Error reason -> Logging.Warn "Error recalculating patterns for %s: %s" entry.Hash reason

                    ctx.Progress(i + 1, charts.Length)
            }
            |> Seq.chunkBySize 1000
            |> Seq.iter(fun chunk ->
                lock chart_db.LockObject (fun () -> DbCharts.update_calculated_data chunk chart_db.Database)
            )
        }

    static member RecalculationNeeded(chart_db: ChartDatabase) : bool = chart_db.RecalculationNeeded
