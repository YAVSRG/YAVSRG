namespace Interlude.Features.Play

open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Gameplay.Mods
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Play.GameplayWidgets
open Prelude.Gameplay.NoteColors

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of chart: ModChart * rate: float32 * ReplayData

type ReplayScreen(mode: ReplayMode) as this =
    inherit Screen()
    
    let keypressData, auto, rate, chart =
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

    let firstNote = offsetOf chart.Notes.First.Value

    let scoringConfig = Rulesets.current
    let scoring = createScoreMetric scoringConfig chart.Keys keypressData chart.Notes rate
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: PlayState =
        {
            Ruleset = scoringConfig
            Scoring = scoring
            HP = scoring.HP
            OnHit = onHit.Publish
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
            Pacemaker = PacemakerInfo.None
        }

    do
        let playfield = Playfield(getColoredChart (noteskinConfig().NoteColors) chart, scoring)
        this.Add playfield

        if noteskinConfig().EnableColumnLight then
            playfield.Add(new ColumnLighting(chart.Keys, noteskinConfig(), widgetHelper))

        if noteskinConfig().Explosions.FadeTime >= 0.0f then
            playfield.Add(new Explosions(chart.Keys, noteskinConfig(), widgetHelper))

        playfield.Add(LaneCover())

        let inline add_widget (constructor: 'T -> Widget) = 
            let config: ^T = getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                let w = constructor config
                w.Position <- { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
                if pos.Float then this.Add w else playfield.Add w

        if not auto then
            add_widget (fun c -> new AccuracyMeter(c, widgetHelper))
            add_widget (fun c -> new HitMeter(c, widgetHelper))
            add_widget (fun c -> new LifeMeter(c, widgetHelper))
            add_widget (fun c -> new JudgementCounts(c, widgetHelper))
        add_widget (fun c -> new ComboMeter(c, widgetHelper))
        add_widget (fun c -> new SkipButton(c, widgetHelper))
        add_widget (fun c -> new ProgressMeter(c, widgetHelper))

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Dialog.close()
        Background.dim (float32 options.BackgroundDim.Value)
        Screen.Toolbar.hide()
        Gameplay.rate.Value <- rate
        Song.changeRate rate
        Song.changeGlobalOffset (toTime options.AudioOffset.Value)
        Song.onFinish <- SongFinishAction.Wait
        Song.playLeadIn()
        Input.finish_frame_events()

    override this.OnExit next =
        Background.dim 0.7f
        Screen.Toolbar.show()

    override this.OnBack() = Some Screen.Type.LevelSelect

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Song.timeWithOffset()
        let chartTime = now - firstNote

        if not keypressData.Finished then scoring.Update chartTime

        if (!|"options").Tapped() then
            QuickOptions.show(scoring, ignore)
        
        if keypressData.Finished then Screen.back Transitions.Flags.Default