namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Calculator
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

type Preview(info: LoadedChartInfo, change_rate: Rate -> unit) as this =
    inherit Dialog()

    let playstate, recreate_scoring = PlayState.Dummy info
    let mutable playstate = playstate
    let mutable recreate_scoring = recreate_scoring
    let mutable last_time = -Time.infinity

    let difficulty_distribution_raw_notes (difficulty: Difficulty) =
        difficulty.NoteDifficulty
        |> Seq.concat
        |> Seq.map _.T
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst

    let difficulty_distribution_notes (difficulty: Difficulty) =
        difficulty.Strain
        |> Seq.map fst
        |> Seq.concat
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst

    let difficulty_distribution_chords (difficulty: Difficulty) =
        difficulty.Strain
        |> Seq.map snd
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst

    let mutable playfield =
        Playfield(info.WithColors, playstate, Content.NoteskinConfig, false)
        |+ LanecoverOverReceptors()

    let mutable timeline = Timeline(info.WithMods, Song.seek, SelectedChart.rate)

    let mutable ddrn = difficulty_distribution_raw_notes info.Rating
    let mutable ddn = difficulty_distribution_notes info.Rating
    let mutable ddc = difficulty_distribution_chords info.Rating
    let mutable ddmax = ddn |> Seq.map snd |> Seq.max |> float32

    let change_chart_listener =
        SelectedChart.on_chart_change_finished.Subscribe(fun info ->
            let _playstate, _recreate_scoring = PlayState.Dummy info
            playstate <- _playstate
            recreate_scoring <- _recreate_scoring
            playfield <-
                Playfield(info.WithColors, playstate, Content.NoteskinConfig, false)
                |+ LanecoverOverReceptors()
            timeline <- Timeline(info.WithMods, Song.seek, SelectedChart.rate)
            ddrn <- difficulty_distribution_raw_notes info.Rating
            ddn <- difficulty_distribution_notes info.Rating
            ddc <- difficulty_distribution_chords info.Rating
            ddmax <- ddn |> Seq.map snd |> Seq.max |> float32
            if this.Initialised then
                playfield.Init this
                timeline.Init this
        )

    let unpause_song_on_exit = Song.playing()

    let volume = Volume()

    override this.Init(parent: Widget) =
        base.Init parent
        playfield.Init this
        timeline.Init this
        volume.Init this

    override this.Draw() =
        playfield.Draw()
        timeline.Draw()
        volume.Draw()

        //let l = this.Bounds.Left + 20.0f
        //for v, count in ddrn do
        //    Render.rect (Rect.Box (l + v * 10.0f, this.Bounds.Top + 200.0f, 5.0f, float32 count / ddmax * 100.0f)) Colors.red
        //for v, count in ddn do
        //    Render.rect (Rect.Box (l + v * 10.0f, this.Bounds.Top + 400.0f, 5.0f, float32 count / ddmax * 100.0f)) Colors.green
        //for v, count in ddc do
        //    Render.rect (Rect.Box (l + v * 10.0f, this.Bounds.Top + 600.0f, 5.0f, float32 count / ddmax * 100.0f)) Colors.blue

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        timeline.Update(elapsed_ms, moved)
        playfield.Update(elapsed_ms, moved)

        if Song.playing() then
            let now = playstate.CurrentChartTime()
            playstate.Scoring.Update now
            if last_time > now then
                recreate_scoring()
            last_time <- now

        if (%%"preview").Tapped() || (%%"exit").Tapped() || Mouse.released Mouse.RIGHT then
            this.Close()
        elif (%%"select").Tapped() then
            this.Close()
            LevelSelect.choose_this_chart ()
        elif (%%"screenshot").Tapped() then
            Toolbar.take_screenshot ()
        elif Screen.current_type = Screen.Type.LevelSelect && (%%"random_chart").Tapped() then
            LevelSelect.random_chart()
        elif Screen.current_type = Screen.Type.LevelSelect && (%%"previous_random_chart").Tapped() then
            LevelSelect.History.back()
        elif Screen.current_type = Screen.Type.LevelSelect && (%%"next").Tapped() then
            Tree.next()
        elif Screen.current_type = Screen.Type.LevelSelect && (%%"previous").Tapped() then
            Tree.previous()
        elif (%%"pause").Tapped() || (%%"pause_music").Tapped() then
            if Song.playing () then
                (if Song.time () > 0.0f<ms> then Song.pause ())
            elif not (Mouse.held Mouse.LEFT) then Song.resume ()
        else
            SelectedChart.change_rate_hotkeys change_rate

        if not (Mouse.held Mouse.LEFT) then
            let scroll = Mouse.scroll()
            if scroll <> 0.0f then
                if Song.playing() then
                    Song.pause()
                    Song.seek(Song.time() - scroll * 40.0f<ms>)
                    Song.resume()
                else
                    Song.seek(Song.time() - scroll * 40.0f<ms>)

    override this.Close() =
        change_chart_listener.Dispose()
        if unpause_song_on_exit then
            Song.resume ()
        base.Close()