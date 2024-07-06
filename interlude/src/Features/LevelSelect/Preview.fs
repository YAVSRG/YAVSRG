namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

type Preview(info: LoadedChartInfo, change_rate: float32 -> unit) =
    inherit Dialog()

    let playfield =
        Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)
        |+ LanecoverOverReceptors()

    let volume = Volume()

    let timeline = Timeline(info.WithMods, Song.seek, SelectedChart.rate)
    let mutable dragging = false

    override this.Init(parent: Widget) =
        base.Init parent
        playfield.Init this
        volume.Init this
        timeline.Init this

    override this.Draw() =
        playfield.Draw()
        volume.Draw()
        timeline.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timeline.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        playfield.Update(elapsed_ms, moved)

        if (%%"preview").Tapped() || (%%"exit").Tapped() || Mouse.released Mouse.RIGHT then
            this.Close()
        elif (%%"select").Tapped() then
            this.Close()
            LevelSelect.choose_this_chart ()
        elif (%%"screenshot").Tapped() then
            Toolbar.take_screenshot ()
        else
            SelectedChart.change_rate_hotkeys change_rate

    override this.Close() =
        if dragging then
            Song.resume ()

        base.Close()
