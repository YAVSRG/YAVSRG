namespace Prelude.Data.Maintenance

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Calculator.Patterns
open Prelude.Data
open Prelude.Data.Library

module Patterns =

    let recalculate (chart_db: ChartDatabase, progress: ProgressCallback) : Async<unit> =
        chart_db.RecalculationNeeded <- false
        async {
            seq {
                let charts = chart_db.Entries |> Seq.toArray
                for i, entry in Seq.indexed charts do
                    match ChartDatabase.get_chart entry.Hash chart_db with
                    | Ok chart ->
                        let difficulty = Difficulty.calculate(1.0f<rate>, chart.Notes)
                        yield entry.Hash, difficulty.Overall, PatternReport.from_chart (difficulty, chart)
                    | Error reason -> Logging.Warn "Error recalculating patterns for %s: %s" entry.Hash reason
                    progress (Processing (i + 1, charts.Length))
            }
            |> Seq.chunkBySize 1000
            |> Seq.iter (fun chunk ->
                lock chart_db.LockObject <| fun () ->
                DbCharts.update_calculated_data chunk chart_db.Database
            )
            progress Complete
        }

    let recalculate_needed (db: ChartDatabase) = db.RecalculationNeeded