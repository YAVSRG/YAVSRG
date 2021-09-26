namespace Prelude.Test

open Prelude.Common
open Prelude.Scoring

module Metrics =

    let main() =
        
        let chart = Imports.getRandomChart()
        let perfectPlayProvider = StoredReplayProvider.AutoPlay(chart.Keys, chart.Notes)

        Logging.Info "Replay round trip!"

        let original = (perfectPlayProvider :> IReplayProvider).GetFullReplay()
        let res = original |> Replay.compress |> Replay.decompress
        assert(original.Length = res.Length)
        for i = 0 to original.Length - 1 do
            assert(original.[i] = res.[i])

        Logging.Info "Replay round trip was a success."

        let metric = ScoreClassifierPlus(4, false, VibeGauge(), chart.Keys, perfectPlayProvider, chart.Notes, 1.0f)
         
        metric.Update Time.infinity

        printfn "%A" metric.State
        printfn "%s" <| metric.HP.Format()