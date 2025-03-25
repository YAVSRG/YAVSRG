namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
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

    let mutable playfield =
        Playfield(info.WithColors, playstate, Content.NoteskinConfig, false)
        |+ LanecoverOverReceptors()

    let mutable timeline = Timeline(info.WithMods, Song.seek, SelectedChart.rate)

    let mutable difficulty_overlay = DifficultyOverlay(info.WithMods, playfield, info.Difficulty, playstate)
    let mutable show_difficulty_overlay = false

    let change_chart_listener =
        SelectedChart.on_chart_change_finished.Subscribe(fun info ->
            let _playstate, _recreate_scoring = PlayState.Dummy info
            playstate <- _playstate
            recreate_scoring <- _recreate_scoring
            playfield <-
                Playfield(info.WithColors, playstate, Content.NoteskinConfig, false)
                |+ LanecoverOverReceptors()
            difficulty_overlay <- DifficultyOverlay(info.WithMods, playfield, info.Difficulty, playstate)
            timeline <- Timeline(info.WithMods, Song.seek, SelectedChart.rate)
            if this.Initialised then
                difficulty_overlay.Init this
                playfield.Init this
                timeline.Init this
        )

    let unpause_song_on_exit = Song.playing()

    let volume = VolumeSlider()

    override this.Init(parent: Widget) =
        Selection.clear()
        base.Init parent
        playfield.Init this
        difficulty_overlay.Init this
        timeline.Init this
        volume.Init this

    override this.Draw() =
        playfield.Draw()
        if show_difficulty_overlay then difficulty_overlay.Draw()
        timeline.Draw()
        volume.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        timeline.Update(elapsed_ms, moved)
        if show_difficulty_overlay then difficulty_overlay.Update(elapsed_ms, moved)
        playfield.Update(elapsed_ms, moved)

        if Song.playing() then
            let now = playstate.CurrentChartTime()
            playstate.Scoring.Update now
            if last_time > now then
                recreate_scoring()
            last_time <- now

        if (%%"preview").Pressed() || (%%"exit").Pressed() || Mouse.released Mouse.RIGHT then
            this.Close()
        elif (%%"select").Pressed() then
            this.Close()
            LevelSelect.choose_this_chart ()
        elif (%%"screenshot").Pressed() then
            Toolbar.take_screenshot ()
        elif Screen.current_type = ScreenType.LevelSelect && (%%"random_chart").Pressed() then
            LevelSelect.random_chart()
        elif Screen.current_type = ScreenType.LevelSelect && (%%"previous_random_chart").Pressed() then
            LevelSelect.History.back()
        elif Screen.current_type = ScreenType.LevelSelect && (%%"next").Pressed() then
            Tree.next()
        elif Screen.current_type = ScreenType.LevelSelect && (%%"previous").Pressed() then
            Tree.previous()
        elif (%%"difficulty_overlay").Pressed() then
            show_difficulty_overlay <- not show_difficulty_overlay
        elif (%%"pause").Pressed() || (%%"pause_music").Pressed() then
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