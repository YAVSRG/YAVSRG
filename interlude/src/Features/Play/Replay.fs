namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Prelude.Charts.Tools
open Prelude.Gameplay
open Prelude.Gameplay.Metrics
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.HUD

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of chart: ModChart * rate: float32 * ReplayData

type InputOverlay(keys, replayData: ReplayData, state: PlayState, playfield: Playfield, enable: Setting<bool>) =
    inherit StaticWidget(NodeType.None)

    let mutable seek = 0
    let keys_down = Array.zeroCreate keys
    let keys_times = Array.zeroCreate keys

    let scrollDirectionPos: float32 -> Rect -> Rect =
        if options.Upscroll.Value then
            fun _ -> id
        else
            fun bottom ->
                fun (r: Rect) ->
                    {
                        Left = r.Left
                        Top = bottom - r.Bottom
                        Right = r.Right
                        Bottom = bottom - r.Top
                    }

    override this.Init(parent) =
        state.ScoringChanged.Publish.Add(fun _ -> seek <- 0)
        base.Init parent

    override this.Draw() =

        if not enable.Value then
            ()
        else

            Draw.rect playfield.Bounds Colors.shadow_2.O2

            let draw_press (k, now: ChartTime, start: ChartTime, finish: ChartTime) =
                let y t =
                    float32 options.HitPosition.Value
                    + float32 (t - now) * (options.ScrollSpeed.Value / Gameplay.rate.Value)
                    + playfield.ColumnWidth * 0.5f

                Rect.Create(
                    playfield.Bounds.Left + playfield.ColumnPositions.[k] + 5.0f,
                    y start,
                    playfield.Bounds.Left + playfield.ColumnPositions.[k] + playfield.ColumnWidth
                    - 5.0f,
                    y finish
                )
                |> scrollDirectionPos playfield.Bounds.Bottom
                |> fun a -> Draw.rect a Colors.grey_2.O2

            let now = state.CurrentChartTime()

            while replayData.Length - 1 > seek
                  && let struct (t, _) = replayData.[seek + 1] in
                     t < now - 100.0f<ms> do
                seek <- seek + 1

            let timeTarget =
                now + 1080.0f<ms> / (options.ScrollSpeed.Value / Gameplay.rate.Value)

            let mutable peek = seek
            let struct (t, b) = replayData.[peek]

            for k = 0 to keys - 1 do
                if Bitmask.hasBit k b then
                    keys_down.[k] <- true
                    keys_times.[k] <- t
                else
                    keys_down.[k] <- false

            while replayData.Length - 1 > peek
                  && let struct (t, _) = replayData.[peek] in
                     t < timeTarget do
                let struct (t, b) = replayData.[peek]

                for k = 0 to keys - 1 do
                    if Bitmask.hasBit k b then
                        if not keys_down.[k] then
                            keys_down.[k] <- true
                            keys_times.[k] <- t
                    else if keys_down.[k] then
                        keys_down.[k] <- false
                        draw_press (k, now, keys_times.[k], t)

                peek <- peek + 1

            for k = 0 to keys - 1 do
                if keys_down.[k] then
                    draw_press (k, now, keys_times.[k], timeTarget)

module ReplayScreen =

    let show_input_overlay = Setting.simple false

    type Controls() =
        inherit StaticContainer(NodeType.None)

        override this.Init(parent) =
            this
            |* IconButton(
                "Toggle input overlay",
                Icons.preview,
                50.0f,
                (fun () -> show_input_overlay.Set(not show_input_overlay.Value)),
                Position = Position.SliceTop(50.0f).Margin(20.0f, 0.0f)
            )

            base.Init parent

        override this.Draw() =
            Draw.rect this.Bounds Colors.black.O2
            base.Draw()

    type ControlOverlay(is_auto, on_seek) =
        inherit DynamicContainer(NodeType.None)

        let mutable show = true
        let mutable show_timeout = 3000.0

        override this.Init(parent) =
            this
            |+ Text(
                (Icons.watch + if is_auto then " Watching AUTOPLAY" else " Watching replay"),
                Color = K Colors.text,
                Align = Alignment.CENTER,
                Position = Position.SliceTop(70.0f).Margin(30.0f, 10.0f).SliceLeft(440.0f)
            )
            |+ Timeline(Gameplay.Chart.WITH_MODS.Value, on_seek)
            |* Controls(Position = Position.Box(0.0f, 0.0f, 30.0f, 70.0f, 440.0f, 60.0f))

            base.Init parent

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            if Mouse.moved_recently () then
                show <- true
                this.Position <- Position.Default
                show_timeout <- 1500.0
            elif show then
                show_timeout <- show_timeout - elapsedTime

                if show_timeout < 0.0 then
                    show <- false

                    this.Position <-
                        { Position.Default with
                            Top = 0.0f %- 300.0f
                            Bottom = 1.0f %+ 100.0f
                        }

    let replay_screen (mode: ReplayMode) =

        let replay_data, is_auto, rate, chart =
            match mode with
            | ReplayMode.Auto ->
                let chart = Gameplay.Chart.WITH_MODS.Value

                StoredReplayProvider.AutoPlay(chart.Keys, chart.Notes) :> IReplayProvider,
                true,
                Gameplay.rate.Value,
                chart
            | ReplayMode.Replay(modchart, rate, data) ->
                StoredReplayProvider(data) :> IReplayProvider, false, rate, modchart

        let firstNote = chart.Notes.[0].Time
        let ruleset = Rulesets.current

        let mutable replay_data = replay_data

        let mutable scoring =
            createScoreMetric ruleset chart.Keys replay_data chart.Notes rate

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider(replay_data.GetFullReplay())
            scoring <- createScoreMetric ruleset chart.Keys replay_data chart.Notes rate
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter

                if not is_auto then
                    add_widget AccuracyMeter
                    add_widget HitMeter
                    add_widget JudgementCounts
                    add_widget JudgementMeter
                    add_widget EarlyLateMeter
                    add_widget RateModMeter
                    add_widget BPMMeter

                this
                |+ InputOverlay(chart.Keys, replay_data.GetFullReplay(), this.State, this.Playfield, show_input_overlay)
                |* ControlOverlay(
                    is_auto,
                    fun t ->
                        let now = Song.time () in
                        Song.seek t

                        if t < now then
                            seek_backwards this
                )

            override this.OnEnter p =
                DiscordRPC.playing ("Watching a replay", Gameplay.Chart.CACHE_DATA.Value.Title)
                base.OnEnter p

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.time_with_offset ()
                let chartTime = now - firstNote

                if not replay_data.Finished then
                    scoring.Update chartTime

                if replay_data.Finished then
                    Screen.back Transitions.Flags.Default
        }
