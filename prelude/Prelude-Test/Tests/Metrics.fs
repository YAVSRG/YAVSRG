namespace Prelude.Test

open Prelude.Common
open Prelude.Scoring

module Metrics =

    let main() =
        
        let chart = Imports.getRandomChart()
        let perfectPlayProvider = StoredReplayProvider.AutoPlay(chart.Keys,  chart.Notes)
        let metric = ScoreClassifier(4, false, chart.Keys, perfectPlayProvider, chart.Notes, 1.0f)

        metric.Update(infinityf * 1.0f<ms>)

        printfn "%A" metric.State
        printfn "%s" <| metric.HP.Format()