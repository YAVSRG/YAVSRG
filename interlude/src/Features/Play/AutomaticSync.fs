namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Gameplay
open Interlude.Features
open Interlude.Options
open Interlude.Utils
open Interlude.UI.Menu

module AutomaticSync =

    let offset = 
        Setting.make
            (fun v -> Gameplay.Chart.saveData.Value.Offset <- v + Gameplay.Chart.current.Value.FirstNote; Song.changeLocalOffset v)
            (fun () -> Gameplay.Chart.saveData.Value.Offset - Gameplay.Chart.current.Value.FirstNote)
        |> Setting.roundt 0

    let apply(scoring: IScoreMetric) =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f
        for ev in scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()
        let mean = sum / count * Gameplay.rate.Value

        let firstNote = Gameplay.Chart.current.Value.FirstNote
        let recommendedOffset = Gameplay.Chart.saveData.Value.Offset - firstNote - mean * 1.2f
        offset.Set recommendedOffset
        Logging.Debug("Offset changed to " + offset.Value.ToString())