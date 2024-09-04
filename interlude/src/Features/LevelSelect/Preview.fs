namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

type Preview(info: LoadedChartInfo, change_rate: float32 -> unit) as this =
    inherit Dialog()

    let mutable playfield =
        Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)
        |+ LanecoverOverReceptors()

    let mutable timeline = Timeline(info.WithMods, Song.seek, SelectedChart.rate)

    let change_chart_listener =
        SelectedChart.on_chart_change_finished.Subscribe(fun info ->
            playfield <-
                Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)
                |+ LanecoverOverReceptors()
            timeline <- Timeline(info.WithMods, Song.seek, SelectedChart.rate)
            if this.Initialised then 
                playfield.Init this
                timeline.Init this
        )

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

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        timeline.Update(elapsed_ms, moved)
        playfield.Update(elapsed_ms, moved)

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
            LevelSelect.History.previous()
        else
            SelectedChart.change_rate_hotkeys change_rate

    override this.Close() =
        change_chart_listener.Dispose()
        if Mouse.held Mouse.LEFT then
            Song.resume ()
        base.Close()
