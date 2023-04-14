namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude.Common
open Prelude.Gameplay.Mods
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Play.GameplayWidgets

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of chart: ModChart * rate: float32 * ReplayData

type InputOverlay(keys, replayData: ReplayData, state: PlayState, playfield: Playfield, enable: Setting<bool>) =
    inherit StaticWidget(NodeType.None)

    let mutable seek = 0
    let keys_down = Array.zeroCreate keys
    let keys_times = Array.zeroCreate keys

    let scrollDirectionPos : float32 -> Rect -> Rect =
        if options.Upscroll.Value then fun _ -> id 
        else fun bottom -> fun (r: Rect) -> { Left = r.Left; Top = bottom - r.Bottom; Right = r.Right; Bottom = bottom - r.Top }

    override this.Draw() =

        if not enable.Value then () else

        Draw.rect playfield.Bounds Colors.shadow_2.O2

        let draw_press(k, now: ChartTime, start: ChartTime, finish: ChartTime) =
            let y t = float32 options.HitPosition.Value + float32 (t - now) * options.ScrollSpeed.Value + playfield.ColumnWidth * 0.5f
            Rect.Create(playfield.Bounds.Left + playfield.ColumnPositions.[k] + 5.0f, y start, playfield.Bounds.Left + playfield.ColumnPositions.[k] + playfield.ColumnWidth - 5.0f, y finish)
            |> scrollDirectionPos playfield.Bounds.Bottom
            |> fun a -> Draw.rect a Colors.grey_2.O2

        let now = state.CurrentChartTime()
        while replayData.Length - 1 > seek && let struct (t, _) = replayData.[seek + 1] in t < now - 100.0f<ms> do
            seek <- seek + 1

        let timeTarget = now + 1080.0f<ms> / options.ScrollSpeed.Value
        let mutable peek = seek
        let struct (t, b) = replayData.[peek]
        for k = 0 to keys - 1 do
            if Bitmap.hasBit k b then 
                keys_down.[k] <- true
                keys_times.[k] <- t
            else keys_down.[k] <- false

        while replayData.Length - 1 > peek && let struct (t, _) = replayData.[peek] in t < timeTarget do
            let struct (t, b) = replayData.[peek]
            for k = 0 to keys - 1 do
                if Bitmap.hasBit k b then 
                    if not keys_down.[k] then 
                        keys_down.[k] <- true
                        keys_times.[k] <- t
                else if keys_down.[k] then
                    keys_down.[k] <- false
                    draw_press(k, now, keys_times.[k], t)
            peek <- peek + 1
        
        for k = 0 to keys - 1 do
            if keys_down.[k] then draw_press(k, now, keys_times.[k], timeTarget)

module ReplayScreen =

    let replay_screen(mode: ReplayMode) =

        let show_input_overlay = Setting.simple false

        let replay_data, is_auto, rate, chart =
            match mode with
            | ReplayMode.Auto -> 
                let chart = Gameplay.Chart.withMods.Value
                StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider,
                true,
                Gameplay.rate.Value,
                chart
            | ReplayMode.Replay (modchart, rate, data) -> 
                StoredReplayProvider(data) :> IReplayProvider,
                false,
                rate,
                modchart
        Gameplay.rate.Value <- rate

        let firstNote = offsetOf chart.Notes.First.Value
        let ruleset = Rulesets.current
        let scoring = createScoreMetric ruleset chart.Keys replay_data chart.Notes rate

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x
                
                if not is_auto then
                    add_widget AccuracyMeter
                    add_widget HitMeter
                    add_widget LifeMeter
                    add_widget JudgementCounts
                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter

                this
                |+ Text((Icons.watch + if is_auto then " Watching AUTOPLAY" else " Watching replay"),
                    Color = K Colors.text,
                    Align = Alignment.LEFT,
                    Position = Position.SliceTop(90.0f).Margin(20.0f, 20.0f))
                |+ InputOverlay(chart.Keys, replay_data.GetFullReplay(), this.State, this.Playfield, show_input_overlay)
                |* IconButton("Toggle input overlay", Icons.preview, 50.0f, (fun () -> show_input_overlay.Set (not show_input_overlay.Value)),
                    Position = Position.TrimTop(90.0f).SliceTop(90.0f).Margin(20.0f, 20.0f).SliceLeft(360.0f))
                //|* Frame(NodeType.None, Fill = K Colors.shadow_2.O3, Border = K Colors.grey_2,
                //    Position = Position.SliceLeft(400.0f).SliceTop(600.0f).Margin(20.0f, 100.0f))

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote
            
                if not replay_data.Finished then scoring.Update chartTime
            
                if (!|"options").Tapped() then
                    QuickOptions.show(scoring, ignore)
                    
                if replay_data.Finished then Screen.back Transitions.Flags.Default
        }