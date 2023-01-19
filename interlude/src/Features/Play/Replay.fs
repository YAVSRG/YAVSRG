namespace Interlude.Features.Play

open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Play.GameplayWidgets

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of rate: float32 * ReplayData

type ReplayScreen(mode: ReplayMode) as this =
    inherit Screen()
    
    let chart = Gameplay.Chart.withMods.Value
    let firstNote = offsetOf chart.Notes.First.Value

    let keypressData, auto, rate =
        match mode with
        | ReplayMode.Auto -> StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider, true, Gameplay.rate.Value
        | ReplayMode.Replay (rate, data) -> StoredReplayProvider(data) :> IReplayProvider, false, rate

    let scoringConfig = Rulesets.current
    let scoring = createScoreMetric scoringConfig chart.Keys keypressData chart.Notes rate
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: Helper =
        {
            ScoringConfig = scoringConfig
            Scoring = scoring
            HP = scoring.HP
            OnHit = onHit.Publish
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
            Pacemaker = PacemakerInfo.None
        }

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if noteskinConfig().EnableColumnLight then
            noteRenderer.Add(new ColumnLighting(chart.Keys, noteskinConfig().ColumnLightTime, widgetHelper))

        if noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(LaneCover())

        let inline add_widget (constructor: 'T -> Widget) = 
            let config: ^T = getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                let w = constructor config
                w.Position <- { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
                if pos.Float then this.Add w else noteRenderer.Add w

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

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Song.timeWithOffset()
        let chartTime = now - firstNote

        if not keypressData.Finished then scoring.Update chartTime

        if (!|"options").Tapped() then
            QuickOptions.show(scoring, ignore)
        
        if keypressData.Finished then Screen.back Transitions.Flags.Default