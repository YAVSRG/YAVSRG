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

type DifficultyGraph =
    {
        Width: float32
        Height: float32
        Count: int
        Values: (float32 * int) array
    }

type DifficultyDetails(difficulty) =
    inherit StaticWidget(NodeType.None)

    let difficulty_distribution_raw_notes (difficulty: Difficulty) =
        difficulty.NoteDifficulty
        |> Seq.concat
        |> Seq.map _.T
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst
        |> Array.ofSeq

    let difficulty_distribution_notes (difficulty: Difficulty) =
        difficulty.Strain
        |> Seq.map fst
        |> Seq.concat
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst
        |> Array.ofSeq

    let difficulty_distribution_chords (difficulty: Difficulty) =
        difficulty.Strain
        |> Seq.map snd
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst
        |> Array.ofSeq

    let graph (data: (float32 * int) array) : DifficultyGraph =
        let max_y = if data.Length > 0 then data |> Seq.map snd |> Seq.max |> float32 else 1.0f
        let max_x = if data.Length > 0 then data |> Seq.map fst |> Seq.max else 1.0f
        let count = data |> Seq.filter (fun (_, count) -> float32 count / max_y > 0.01f) |> Seq.length
        { Height = max_y; Width = max_x; Count = count; Values = data }

    let draw_graph (y: float32) (color: Color) (d: DifficultyGraph) =
        for v, count in d.Values do
            Render.rect (Rect.Box (20.0f + v * 10.0f, y, 5.0f, float32 count / d.Height * 100.0f)) color
        Text.draw(Style.font, d.Count.ToString(), 20.0f, 20.0f, y - 30.0f, Colors.white)
        Text.draw(Style.font, sprintf "%.2f" d.Width, 20.0f, 420.0f, y - 30.0f, Colors.white)

    let mutable ddrn = graph [||]
    let mutable ddn = graph [||]
    let mutable ddc = graph [||]

    member this.SetData(difficulty: Difficulty) =
        ddrn <- difficulty_distribution_raw_notes difficulty |> graph
        ddn <- difficulty_distribution_notes difficulty |> graph
        ddc <- difficulty_distribution_chords difficulty |> graph

    override this.Init (parent: Widget) =
        this.SetData difficulty
        base.Init parent

    override this.Draw() =
        draw_graph 200.0f Colors.red ddrn
        draw_graph 400.0f Colors.green ddn
        draw_graph 600.0f Colors.blue ddc

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

    let diff_stuff = DifficultyDetails(info.Rating)

    let change_chart_listener =
        SelectedChart.on_chart_change_finished.Subscribe(fun info ->
            let _playstate, _recreate_scoring = PlayState.Dummy info
            playstate <- _playstate
            recreate_scoring <- _recreate_scoring
            playfield <-
                Playfield(info.WithColors, playstate, Content.NoteskinConfig, false)
                |+ LanecoverOverReceptors()
            timeline <- Timeline(info.WithMods, Song.seek, SelectedChart.rate)
            diff_stuff.SetData(info.Rating)
            if this.Initialised then
                playfield.Init this
                timeline.Init this
        )

    let unpause_song_on_exit = Song.playing()

    let volume = Volume()

    override this.Init(parent: Widget) =
        base.Init parent
        playfield.Init this
        diff_stuff.Init this
        timeline.Init this
        volume.Init this

    override this.Draw() =
        playfield.Draw()
        //diff_stuff.Draw()
        timeline.Draw()
        volume.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        timeline.Update(elapsed_ms, moved)
        diff_stuff.Update(elapsed_ms, moved)
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