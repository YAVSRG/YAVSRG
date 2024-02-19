namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Tools
open Prelude.Charts.Tools.NoteColors
open Prelude.Data.Scores
open Prelude.Gameplay
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.HUD
open Interlude.Features.Score

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto of ColoredChart
    | Replay of score_info: ScoreInfo * with_colors: ColoredChart

type private HitOverlay
    (
        rate: float32,
        chart: ModdedChart,
        replay_data: ReplayData,
        state: PlayState,
        playfield: Playfield,
        enable: Setting<bool>
    ) =
    inherit StaticWidget(NodeType.None)

    let hit_events =
        let full_score =
            Metrics.run state.Ruleset chart.Keys (StoredReplayProvider replay_data) chart.Notes rate

        full_score.HitEvents |> Array.ofSeq

    let mutable seek = 0

    let scroll_direction_pos: float32 -> Rect -> Rect =
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
            let draw_event (now: ChartTime) (ev: HitEvent<HitEventGuts>) =
                let y t =
                    float32 options.HitPosition.Value
                    + float32 (t - now) * (options.ScrollSpeed.Value / Gameplay.rate.Value)
                    + playfield.ColumnWidth * 0.5f

                let delta =
                    match ev.Guts with
                    | Hit x -> x.Delta
                    | Release x -> x.Delta

                let is_miss =
                    match ev.Guts with
                    | Hit x -> x.Missed
                    | Release x -> x.Missed

                let color =
                    match ev.Guts with
                    | Hit x ->
                        match x.Judgement with
                        | None -> Colors.grey_1.O2
                        | Some i -> state.Ruleset.JudgementColor i
                    | Release x ->
                        match x.Judgement with
                        | None -> Colors.grey_1.O2
                        | Some i -> state.Ruleset.JudgementColor i

                if is_miss then
                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y (ev.Time - state.Ruleset.Accuracy.MissWindow),
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y (ev.Time - state.Ruleset.Accuracy.MissWindow)
                        )
                        .Shrink(0.0f, -playfield.ColumnWidth * 0.5f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Text.fill_b (Style.font, Icons.X, a, (color, Colors.black), 0.5f)
                else
                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y ev.Time,
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y (ev.Time - delta)
                        )
                        .Shrink((playfield.ColumnWidth - 5.0f) * 0.5f, 0.0f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Draw.rect a color

                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y ev.Time,
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y ev.Time
                        )
                        .Shrink(20.0f, -2.5f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Draw.rect a color

            let now =
                state.CurrentChartTime()
                + Performance.frame_compensation ()
                + options.VisualOffset.Value * 1.0f<ms> * Gameplay.rate.Value

            while hit_events.Length - 1 > seek && hit_events.[seek + 1].Time < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now
                + (1080.0f<ms> + state.Ruleset.Accuracy.MissWindow)
                  / (options.ScrollSpeed.Value / Gameplay.rate.Value)

            let mutable peek = seek

            while hit_events.Length - 1 > peek && hit_events.[peek].Time < until_time do
                draw_event now hit_events.[peek]
                peek <- peek + 1

type private InputOverlay(keys, replay_data: ReplayData, state: PlayState, playfield: Playfield, enable: Setting<bool>)
    =
    inherit StaticWidget(NodeType.None)

    let mutable seek = 0
    let keys_down = Array.zeroCreate keys
    let keys_times = Array.zeroCreate keys

    let scroll_direction_pos: float32 -> Rect -> Rect =
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

            let draw_press (k, now: ChartTime, start: ChartTime, finish: ChartTime) =
                let y t =
                    float32 options.HitPosition.Value
                    + float32 (t - now) * (options.ScrollSpeed.Value / Gameplay.rate.Value)
                    + playfield.ColumnWidth * 0.5f

                Rect
                    .Create(
                        playfield.Bounds.Left + playfield.ColumnPositions.[k],
                        y start,
                        playfield.Bounds.Left + playfield.ColumnPositions.[k] + playfield.ColumnWidth,
                        y finish
                    )
                    .Shrink(20.0f, 0.0f)
                |> scroll_direction_pos playfield.Bounds.Bottom
                |> fun a -> Draw.rect a Colors.grey_2.O2

            let now =
                state.CurrentChartTime()
                + Performance.frame_compensation ()
                + options.VisualOffset.Value * 1.0f<ms> * Gameplay.rate.Value

            while replay_data.Length - 1 > seek
                  && let struct (t, _) = replay_data.[seek + 1] in
                     t < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now + 1080.0f<ms> / (options.ScrollSpeed.Value / Gameplay.rate.Value)

            let mutable peek = seek
            let struct (t, b) = replay_data.[peek]

            for k = 0 to keys - 1 do
                if Bitmask.has_key k b then
                    keys_down.[k] <- true
                    keys_times.[k] <- t
                else
                    keys_down.[k] <- false

            while replay_data.Length - 1 > peek
                  && let struct (t, _) = replay_data.[peek] in
                     t < until_time do
                let struct (t, b) = replay_data.[peek]

                for k = 0 to keys - 1 do
                    if Bitmask.has_key k b then
                        if not keys_down.[k] then
                            keys_down.[k] <- true
                            keys_times.[k] <- t
                    else if keys_down.[k] then
                        keys_down.[k] <- false
                        draw_press (k, now, keys_times.[k], t)

                peek <- peek + 1

            for k = 0 to keys - 1 do
                if keys_down.[k] then
                    draw_press (k, now, keys_times.[k], until_time)

module ReplayScreen =

    type private ReplayState =
        {
            ShowInputOverlay: Setting<bool>
            ShowHitOverlay: Setting<bool>
            PlayfieldDim: Setting.Bounded<float32>
            IsAuto: bool
        }

    let private controls (state: ReplayState) : SlideoutContent =

        let overlay_buttons =
            NavigationContainer.Column<Button>(Position = Position.SliceLeft(400.0f))
            |+ Button(
                (fun () ->
                    (if state.ShowInputOverlay.Value then
                            Icons.CHECK_CIRCLE
                        else
                            Icons.CIRCLE)
                    + " Input overlay"
                ),
                (fun () -> Setting.app not state.ShowInputOverlay),
                Position = Position.SliceTop(50.0f)
            )
            |+ Button(
                (fun () ->
                    (if state.ShowHitOverlay.Value then
                            Icons.CHECK_CIRCLE
                        else
                            Icons.CIRCLE)
                    + " Hit overlay"
                ),
                (fun () -> Setting.app not state.ShowHitOverlay),
                Position = Position.SliceBottom(50.0f)
            )

        let dim_slider =
            Menu.Slider.Percent(
                state.PlayfieldDim,
                Position = Position.TrimLeft(400.0f).SliceLeft(400.0f).SliceBottom(50.0f).Margin(5.0f)
            )

        SlideoutContent(
            NavigationContainer.Row<Widget>()
            |+ overlay_buttons
            |+ dim_slider,
            100.0f
        )
        |+ Text(
            (Icons.PLAY + if state.IsAuto then " Autoplay" else " Watching replay"),
            Color = K Colors.text,
            Align = Alignment.RIGHT,
            Position = Position.Margin(30.0f, 20.0f)
        )
        |+ Text(
            "Playfield dim",
            Color =
                (fun () ->
                    if dim_slider.Focused then
                        Colors.text_yellow_2
                    elif state.ShowInputOverlay.Value || state.ShowHitOverlay.Value then
                        Colors.text
                    else
                        Colors.text_greyout
                ),
            Align = Alignment.CENTER,
            Position = Position.TrimLeft(400.0f).SliceLeft(400.0f).SliceTop(50.0f)
        )

    type private ControlOverlay(with_mods: ModdedChart, state: ReplayState, on_seek: Time -> unit) =
        inherit DynamicContainer(NodeType.None)

        let mutable show = true
        let mutable show_timeout = 3000.0

        let replay_controls = controls state
        let slideout = Slideout(replay_controls, AutoCloseWhen = K false)

        override this.Init(parent) =
            this 
            |+ Timeline(with_mods, on_seek)
            |* slideout

            base.Init parent

            slideout.Open()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            if Mouse.moved_recently () then
                show <- true
                slideout.Open()
                this.Position <- Position.Default
                show_timeout <- 1500.0

            elif show then
                show_timeout <- show_timeout - elapsed_ms

                if show_timeout < 0.0 then
                    show <- false

                    slideout.Close()

                    this.Position <-
                        { Position.Default with
                            Bottom = 1.0f %+ 100.0f
                        }

            if show && not replay_controls.Focused then
                Screen.back Transitions.Flags.Default |> ignore

    let replay_screen (chart: Chart, mode: ReplayMode) =

        let replay_data, is_auto, rate, with_colors =
            match mode with
            | ReplayMode.Auto with_colors ->
                StoredReplayProvider.AutoPlay(with_colors.Keys, with_colors.Source.Notes) :> IReplayProvider,
                true,
                Gameplay.rate.Value,
                with_colors
            | ReplayMode.Replay(score_info, with_colors) ->
                StoredReplayProvider(score_info.Replay) :> IReplayProvider, false, score_info.Rate, with_colors

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let state = 
            {
                ShowInputOverlay = Setting.simple false
                ShowHitOverlay = Setting.simple false
                PlayfieldDim = Setting.percentf 0.5f
                IsAuto = is_auto
            }

        let playback_speed =
            Setting.bounded rate 0.5f 2.0f |> Setting.trigger (fun r -> Song.change_rate r)

        let mutable replay_data = replay_data

        let mutable scoring = Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider(replay_data.GetFullReplay())
            scoring <- Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter

                if not is_auto then
                    add_widget AccuracyMeter
                    add_widget (fun x -> Conditional((fun () -> not state.ShowHitOverlay.Value), HitMeter x))
                    add_widget JudgementCounts
                    add_widget JudgementMeter
                    add_widget EarlyLateMeter
                    add_widget RateModMeter
                    add_widget BPMMeter

                this
                |+ { new StaticWidget(NodeType.None) with
                       override _.Draw() =
                           if state.ShowInputOverlay.Value || state.ShowHitOverlay.Value then
                               Draw.rect this.Playfield.Bounds (Colors.black.O4a(255.0f * state.PlayfieldDim.Value |> int))
                   }
                |+ InputOverlay(with_colors.Keys, replay_data.GetFullReplay(), this.State, this.Playfield, state.ShowInputOverlay)
                |+ HitOverlay(rate, with_colors.Source, replay_data.GetFullReplay(), this.State, this.Playfield, state.ShowHitOverlay)
                |* ControlOverlay(
                    with_colors.Source,
                    state,
                    fun t ->
                        let now = Song.time () in
                        Song.seek t

                        if t < now then
                            seek_backwards this
                )

            override this.OnEnter p =
                DiscordRPC.playing ("Watching a replay", Gameplay.Chart.CACHE_DATA.Value.Title)
                base.OnEnter p

            override this.OnExit p =
                base.OnExit p
                Song.change_rate Gameplay.rate.Value

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if not replay_data.Finished then
                    scoring.Update chart_time

                if replay_data.Finished then
                    match mode with
                    | ReplayMode.Auto _ -> Screen.back Transitions.Flags.Default |> ignore
                    | ReplayMode.Replay(score_info, _) ->
                        Screen.change_new
                            (fun () ->
                                new ScoreScreen(score_info, ImprovementFlags.Default, false) :> Screen
                            )
                            Screen.Type.Score
                            Transitions.Flags.Default
                        |> ignore

                if (%%"skip").Tapped() && Song.time () > 0.0f<ms> then
                    if Song.playing () then Song.pause () else Song.resume ()
                elif (%%"uprate_small").Tapped() then
                    playback_speed.Value <- playback_speed.Value + 0.01f
                elif (%%"uprate_half").Tapped() then
                    playback_speed.Value <- playback_speed.Value + 0.05f
                elif (%%"uprate").Tapped() then
                    playback_speed.Value <- playback_speed.Value + 0.1f
                elif (%%"downrate_small").Tapped() then
                    playback_speed.Value <- playback_speed.Value - 0.01f
                elif (%%"downrate_half").Tapped() then
                    playback_speed.Value <- playback_speed.Value - 0.05f
                elif (%%"downrate").Tapped() then
                    playback_speed.Value <- playback_speed.Value - 0.1f
        }
