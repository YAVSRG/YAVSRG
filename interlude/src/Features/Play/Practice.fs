namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools.Patterns
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Play.HUD

module PracticeScreen =

    type Timeline(chart: Chart, on_seek: Time -> unit) =
        inherit StaticWidget(NodeType.None)

        let density_graph_1, density_graph_2 = Analysis.density 100 chart
        let density_graph_1, density_graph_2 = Array.map float32 density_graph_1, Array.map float32 density_graph_2
        let max_note_density = Array.max density_graph_1

        override this.Draw() =
            let b = this.Bounds.Shrink(10.0f, 20.0f)
            let start = chart.FirstNote - Song.LEADIN_TIME
        
            let w = b.Width / float32 density_graph_1.Length
            for i = 0 to density_graph_1.Length - 1 do
                let h = 80.0f * density_graph_1.[i] / max_note_density
                let h2 = 80.0f * density_graph_2.[i] / max_note_density
                Draw.rect (Rect.Box(b.Left + float32 i * w, b.Bottom - h, w, h - 5.0f)) (Color.FromArgb(120, Color.White))
                Draw.rect (Rect.Box(b.Left + float32 i * w, b.Bottom - h2, w, h2 - 5.0f)) (Style.color(80, 1.0f, 0.0f))
        
            let percent = (Song.time() - start) / (chart.LastNote - start) 
            Draw.rect (b.SliceBottom(5.0f)) (Color.FromArgb(160, Color.White))
            let x = b.Width * percent
            Draw.rect (b.SliceBottom(5.0f).SliceLeft x) (Style.color(255, 1.0f, 0.0f))

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if this.Bounds.Bottom - Mouse.y() < 200.0f && Mouse.leftClick() then
                let percent = (Mouse.x() - 10.0f) / (Viewport.vwidth - 20.0f) |> min 1.0f |> max 0.0f
                let start = chart.FirstNote - Song.LEADIN_TIME
                let newTime = start + (chart.LastNote - start) * percent
                on_seek newTime

    let info_callout = 
        Callout.Small
            .Title("Practice mode")
            .Icon(Icons.goal)
            .Body("Practise a particular section of a chart by setting the marker in the timeline, and then press play")
            .Hotkey("Play", "skip")
            .Hotkey("Restart", "retry")
            .Hotkey("Show options", "options")
    
    let rec practice_screen (practice_point: Time) =

        let chart = Gameplay.Chart.withMods.Value
        let lastNote = offsetOf chart.Notes.Last.Value - 5.0f<ms> - Song.LEADIN_TIME * Gameplay.rate.Value
        let mutable practice_point = min lastNote practice_point

        let mutable playable_notes = TimeData(chart.Notes.EnumerateBetween (practice_point + Song.LEADIN_TIME * Gameplay.rate.Value) Time.infinity)
        let mutable firstNote = offsetOf playable_notes.First.Value
        let mutable liveplay = LiveReplayProvider firstNote
        let scoring = createScoreMetric Rulesets.current chart.Keys liveplay playable_notes Gameplay.rate.Value

        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable inputKeyState = 0us

        // options state
        let mutable options_mode = true

        let restart(screen: IPlayScreen) =
            liveplay <- LiveReplayProvider firstNote
            let scoring = createScoreMetric Rulesets.current chart.Keys liveplay playable_notes Gameplay.rate.Value
            screen.State.ChangeScoring scoring
            Song.playFrom(practice_point)

        let play(screen: IPlayScreen) =
            playable_notes <- TimeData(chart.Notes.EnumerateBetween (practice_point + Song.LEADIN_TIME * Gameplay.rate.Value) Time.infinity)
            firstNote <- offsetOf playable_notes.First.Value
            screen.FirstNote <- firstNote
            restart(screen)
            options_mode <- false

        let pause(screen: IPlayScreen) =
            Song.pause()
            options_mode <- true

        let options_ui =
            StaticContainer(NodeType.None)
            |+ Timeline(Gameplay.Chart.current.Value, fun t -> practice_point <- min lastNote t; Song.seek t)
            |+ Callout.frame (info_callout) ( fun (w, h) -> Position.Box(0.0f, 0.0f, 20.0f, 20.0f, w, h + 40.0f) )

        { new IPlayScreen(chart, PacemakerInfo.None, Rulesets.current, scoring, FirstNote = firstNote) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x

                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget LifeMeter
                add_widget ComboMeter
                add_widget ProgressMeter
                add_widget JudgementCounts

                this.Add(Conditional((fun () -> options_mode), options_ui))

            override this.OnEnter(p) =
                base.OnEnter(p)
                Song.seek(practice_point)
                Song.pause()

            override this.OnBack() =
                if not options_mode then
                    pause(this)
                    inputKeyState <- 0us
                    None
                else base.OnBack()

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if not (liveplay :> IReplayProvider).Finished then
                    Input.consumeGameplay(binds, fun column time isRelease ->
                        if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                        else inputKeyState <- Bitmap.setBit column inputKeyState
                        liveplay.Add(time, inputKeyState) )
                    this.State.Scoring.Update chartTime

                if (!|"retry").Tapped() then
                    if options_mode then play(this) else restart(this)

                elif options_mode && (!|"skip").Tapped() then
                    play(this)

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    pause(this)
        }

    // ui in OPTIONS MODE:
    // show cursor
    // audio is paused for now
    // timeline in the bottom with density graph
    // timeline seeking sets starting point
    // sync tools: when audio is muted
    // scroll speed
    // hit position
    // visual offset
    // sync tools: when audio is on
    // local offset
    // global offset

    // top left: box explaining stuff
    
    // ui in PLAY MODE:
    // 
    