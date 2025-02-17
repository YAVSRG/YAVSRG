namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay.Replays
open Prelude.Skins.Noteskins
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type private DifficultyOverlay
    (
        chart: ModdedChart,
        difficulty: DifficultyRating,
        state: PlayState,
        playfield: Playfield
    ) =
    inherit StaticWidget(NodeType.None)

    let first_note = chart.FirstNote
    let mutable seek = 0
    let mutable last_time = 0.0f<ms>

    let indicator = Animation.Fade 0.0f

    let scroll_direction_pos: float32 -> float32 -> float32 =
        if options.Upscroll.Value then
            fun _ -> id
        else
            fun bottom -> fun x -> bottom - x

    let draw_label (bounds: Rect) (value: float32) (color: Color) =
        Render.rect bounds Colors.shadow_2.O3
        Text.fill_b(Style.font, sprintf "%.2f" value, bounds.Shrink(10.0f, 5.0f), (color, Colors.shadow_2), Alignment.CENTER)

    let STREAM_SCALE = 15000.0f<ms / rate> * DifficultyRating.STREAM_CURVE_WIDTH_SCALE / DifficultyRating.STREAM_CURVE_HEIGHT_SCALE
    let JACK_SCALE = 15000.0f<ms / rate> * DifficultyRating.JACK_CURVE_WIDTH_SCALE / DifficultyRating.JACK_CURVE_HEIGHT_SCALE

    let draw_row (now: ChartTime) (index: int) =
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
                    (difficulty.NoteDifficulty.[index].[k].SLF * STREAM_SCALE)
                    Colors.cyan_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentR(0.5f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].SRF * STREAM_SCALE)
                    Colors.cyan_accent
                draw_label
                    (note_box.ShrinkPercentB(0.5f).SlicePercentB(0.6f).SlicePercentL(0.5f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].SLB * STREAM_SCALE)
                    Colors.cyan_accent
                draw_label
                    (note_box.ShrinkPercentB(0.5f).SlicePercentB(0.6f).SlicePercentR(0.5f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].SRB * STREAM_SCALE)
                    Colors.cyan_accent

                draw_label
                    (note_box.ShrinkPercentB(0.5f).SlicePercentB(0.6f).TranslateY(-playfield.ColumnWidth * 0.3f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].JB * JACK_SCALE)
                    Colors.green_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).TranslateY(playfield.ColumnWidth * 0.3f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].JF * JACK_SCALE)
                    Colors.green_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentL(0.5f).TranslateY(playfield.ColumnWidth * 0.6f).Shrink(5.0f))
                    (difficulty.NoteDifficulty.[index].[k].T * JACK_SCALE)
                    Colors.pink_accent
                draw_label
                    (note_box.ShrinkPercentT(0.5f).SlicePercentT(0.6f).SlicePercentR(0.5f).TranslateY(playfield.ColumnWidth * 0.6f).Shrink(5.0f))
                    ((fst difficulty.Strain.[index]).[k])
                    Colors.red_accent

        draw_label (note_area.BorderR(playfield.ColumnWidth).SlicePercentY(0.35f).Shrink(5.0f)) (snd difficulty.Strain.[index]) Colors.red_accent

    override this.Init(parent) =
        state.ScoringChanged.Publish.Add(fun _ -> seek <- 0)
        base.Init parent

    override this.Draw() =
        if show_difficulty_overlay.Value then
            let now =
                state.CurrentChartTime()
                + (
                    (if Song.playing() then GameThread.frame_compensation () else 0.0f<ms / rate>)
                    + options.VisualOffset.Value
                ) * Song.playback_rate()
                + first_note

            while chart.Notes.Length - 1 > seek && chart.Notes.[seek + 1].Time < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

            let mutable peek = seek

            while chart.Notes.Length - 1 > peek && chart.Notes.[peek].Time < until_time do
                draw_row now peek
                peek <- peek + 1

            Render.rect (playfield.Bounds.BorderL(50.0f).SlicePercentB(indicator.Value / 14.0f)) Colors.red_accent

    override this.Update (elapsed_ms, moved): unit =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while seek > 0 && chart.Notes.[seek].Time > time do
                seek <- seek - 1

        indicator.Target <- float32 (snd difficulty.Strain.[seek])
        indicator.Update elapsed_ms
        last_time <- state.CurrentChartTime()