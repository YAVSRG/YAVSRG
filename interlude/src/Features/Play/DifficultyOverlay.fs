namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Calculator
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type DifficultyGraph =
    {
        Width: float32
        Height: float32
        Count: int
        Values: (float32 * int) array
    }

type DifficultyOverlay(chart: ModdedChart, playfield: Playfield, difficulty: Difficulty, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let first_note = chart.FirstNote
    let mutable seek = 0
    let mutable last_time = state.CurrentChartTime()

    let scroll_direction_pos: float32 -> float32 -> float32 =
        if options.Upscroll.Value then
            fun _ -> id
        else
            fun bottom -> fun x -> bottom - x

    let draw_label (bounds: Rect) (value: float32) (color: Color) =
        Render.rect bounds Colors.shadow_2.O3
        Text.fill_b(Style.font, sprintf "%.2f" value, bounds.Shrink(10.0f, 5.0f), (color, Colors.shadow_2), Alignment.CENTER)

    let draw_row (now: Time) (index: int) =
        let centre =
            options.HitPosition.Value
            + (chart.Notes.[index].Time - now) * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
            + playfield.ColumnWidth * 0.5f
            |> scroll_direction_pos playfield.Bounds.Bottom

        let note_area = Rect.Create(playfield.Bounds.Left, centre - playfield.ColumnWidth * 0.5f, playfield.Bounds.Right, centre + playfield.ColumnWidth * 0.5f)

        for k = 0 to chart.Keys - 1 do
            if chart.Notes.[index].Data.[k] = NoteType.NORMAL || chart.Notes.[index].Data.[k] = NoteType.HOLDHEAD then
                let note_box = note_area.SliceL(playfield.ColumnPositions.[k], playfield.ColumnWidth)

                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentL(0.5f).Shrink(5.0f))
                    difficulty.NoteDifficulty.[index].[k].SL
                    Colors.cyan_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentR(0.5f).Shrink(5.0f))
                    difficulty.NoteDifficulty.[index].[k].SR
                    Colors.cyan_accent

                draw_label
                    (note_box.ShrinkPercentB(0.5f).SlicePercentB(0.6f).Shrink(5.0f))
                    difficulty.NoteDifficulty.[index].[k].J
                    Colors.green_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentL(0.5f).TranslateY(playfield.ColumnWidth * 0.3f).Shrink(5.0f))
                    difficulty.Strains.[index].NotesV1.[k]
                    Colors.pink_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentR(0.5f).TranslateY(playfield.ColumnWidth * 0.3f).Shrink(5.0f))
                    difficulty.Strains.[index].StrainV1Notes.[k]
                    Colors.red_accent

    let graph (data: (float32 * int) array) : DifficultyGraph =
        let max_y = if data.Length > 0 then data |> Seq.map snd |> Seq.max |> float32 else 1.0f
        let max_x = if data.Length > 0 then data |> Seq.map fst |> Seq.max else 1.0f
        let count = data |> Seq.filter (fun (_, count) -> float32 count / max_y > 0.01f) |> Seq.length
        { Height = max_y; Width = max_x; Count = count; Values = data }

    let draw_graph (y: float32) (color: Color) (d: DifficultyGraph) =
        for v, count in d.Values do
            Render.rect (Rect.Box (20.0f + v * 2.0f, y, 1.0f, float32 count / d.Height * 100.0f)) color
        Text.draw(Style.font, d.Count.ToString(), 20.0f, 20.0f, y - 30.0f, Colors.white)
        Text.draw(Style.font, sprintf "%.2f" d.Width, 20.0f, 420.0f, y - 30.0f, Colors.white)

    let draw_live_data (y: float32) (color: Color) (data: float32 seq) =
        let mutable x = Render.width() - 20.0f
        for d in Seq.truncate 100 data do
            Render.rect (Rect.Box (x - 5.0f, y, 5.0f, d * 0.5f)) color
            x <- x - 5.0f

    let draw_octaves (y: float32) =
        let mutable x = Render.width() - 20.0f
        for (burst, stamina) in
            seq {
                let mutable peek = seek
                while peek >= 0 do
                    yield difficulty.Hands.[peek]
                    peek <- peek - 1
            }
            |> Seq.map _.Left
            |> Seq.filter (fun (x, _ ) ->  x > 0.0f)
            |> Seq.truncate 100
            do
                Render.rect (Rect.Box (x - 5.0f, y, 5.0f, (stamina * 0.125f + burst * 0.875f) * 0.5f)) Colors.red_accent
                Render.rect (Rect.Box (x - 5.0f, y, 5.0f, stamina * 0.125f * 0.5f)) Colors.red_shadow
                x <- x - 5.0f

    let note_difficulties =
        seq {
            let mutable peek = seek
            while peek >= 0 do
                yield! difficulty.Strains.[peek].NotesV1
                peek <- peek - 1
        }
        |> Seq.filter (fun x -> x > 0.0f)

    let note_strains =
        seq {
            let mutable peek = seek
            while peek >= 0 do
                yield! difficulty.Strains.[peek].StrainV1Notes
                peek <- peek - 1
        }
        |> Seq.filter (fun x -> x > 0.0f)

    let full_score_events =
        let x = state.Scoring.Recreate()
        x.Update Time.infinity
        x

    let accuracies =
        let timeline = Performance.acc_timeline difficulty full_score_events
        seq {
            let mutable peek = seek
            while peek >= 0 do
                yield (timeline.[peek] - 0.9f) * 2000.0f
                peek <- peek - 1
        }

    override this.Draw() =
        let now =
            state.CurrentChartTime() +
            first_note +
            (GameThread.frame_compensation () + options.VisualOffset.Value) * Song.playback_rate()

        while chart.Notes.Length - 1 > seek && chart.Notes.[seek + 1].Time < now - 100.0f<ms> do
            seek <- seek + 1

        let until_time =
            now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

        let mutable peek = seek

        while chart.Notes.Length - 1 > peek && chart.Notes.[peek].Time < until_time do
            draw_row now peek
            peek <- peek + 1

        //draw_graph 200.0f Colors.red difficulty_distribution_raw_notes
        //draw_graph 400.0f Colors.green difficulty_distribution_notes
        //draw_graph 600.0f Colors.blue difficulty_distribution_chords

        draw_live_data 200.0f Colors.red note_difficulties
        draw_live_data 400.0f Colors.blue note_strains
        let (burst, stamina) = difficulty.Hands.[seek].Right in
            Text.draw(Style.font, sprintf "R: %.0f | %.0f" burst stamina, 20.0f, this.Bounds.Right - 200.0f, 770.0f, Colors.white)
        let (burst, stamina) = difficulty.Hands.[seek].Left in
            Text.draw(Style.font, sprintf "L: %.0f | %.0f" burst stamina, 20.0f, this.Bounds.Right - 400.0f, 770.0f, Colors.white)
        draw_octaves 600.0f
        draw_live_data 800.0f Colors.yellow_accent accuracies

        Text.draw(Style.font, sprintf "X: %.2f" difficulty.Variety.[peek], 20.0f, this.Bounds.Right - 200.0f, 170.0f, Colors.white)
        Text.draw(Style.font, sprintf "V1: %.2f" difficulty.Overall, 20.0f, this.Bounds.Right - 400.0f, 170.0f, Colors.white)

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while seek > 0 && chart.Notes.[seek].Time > time do
                seek <- seek - 1

        last_time <- time