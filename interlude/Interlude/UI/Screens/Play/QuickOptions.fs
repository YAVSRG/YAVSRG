namespace Interlude.UI.Screens.Play

open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module QuickOptions =

    let page(scoring : Prelude.Scoring.IScoreMetric, callback) : SelectionPage =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f
        for ev in scoring.HitEvents do
            match ev.Guts with
            | Prelude.Scoring.HitEventGuts.Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()
        let mean = sum / count * Gameplay.rate.Value
        {
            Content = fun add ->
                let firstNote = Gameplay.currentChart.Value.FirstNote
                column [
                    PrettySetting("quick.localoffset",
                        Slider(
                            Setting.make
                                (fun v -> Gameplay.chartSaveData.Value.Offset <- toTime v + firstNote; Audio.changeLocalOffset(toTime v))
                                (fun () -> float (Gameplay.chartSaveData.Value.Offset - firstNote))
                            |> Setting.bound -200.0 200.0
                            |> Setting.round 0, 0.01f)
                    ).Position(200.0f)
                    TextBox(
                        (if Time.Abs mean > 1.0f<ms> then K (sprintf "Suggested: %.0f" (Gameplay.chartSaveData.Value.Offset - firstNote - mean))
                        else K ""),
                        K (Color.White, Color.Black),
                        1.0f
                    ) |> positionWidget(-600.0f, 1.0f, 200.0f, 0.0f, -50.0f, 1.0f, 200.0f + PRETTYHEIGHT, 0.0f)
                    PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Position(280.0f)
                    PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Position(360.0f)
                    PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Position(440.0f)
                ] :> Selectable
            Callback = callback
        }

    let show(scoring, callback) = SelectionMenu(N"quick", page(scoring, callback)).Show()