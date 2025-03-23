namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Mods
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.UI

#nowarn "3370"

type Sidebar(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let show_more_info = Setting.simple false

    let mod_string = ModState.format (score_info.Rate, score_info.Mods)

    let category, main_clusters =
        let c = score_info.ChartMeta.Patterns
        c.Category,
        c.ImportantClusters |> Seq.truncate 3 |> Seq.map (fun c -> c.Format score_info.Rate) |> String.concat ", "

    override this.Init(parent) =
        this
        |+ Text(
            sprintf "%s  %s" Icons.ZAP mod_string,
            Position = Position.SliceT(90.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            (fun () -> sprintf "%s  %iK  •  %s" Icons.BAR_CHART score_info.Chart.Keys score_info.Ruleset.Name),
            Position = Position.ShrinkT(90.0f).SliceT(70.0f).ShrinkX(25.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )
        |+ Text(
            sprintf "%s %.2f" Icons.STAR score_info.Rating.Overall,
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            (fun () -> sprintf "%ix" score_info.Scoring.BestCombo),
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            sprintf "%.2f" score_info.Physical,
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.RIGHT
        )

        |+ Button(
            (fun () -> sprintf "MA: %s  •  PA: %s  •  M: %.2fms  •  SD: %.2fms" (!stats).MA (!stats).PA (!stats).TapMean (!stats).TapStandardDeviation),
            (fun () -> show_more_info.Set true),
            Position = Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f),
            TextColor = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text
        )
            .Conditional(show_more_info.Get >> not)
        |+ Text(
            category,
            Position = Position.ShrinkB(85.0f).SliceB(60.0f).ShrinkX(25.0f),
            Align = Alignment.LEFT
        )
            .Conditional(show_more_info.Get >> not)
        |+ Text(
            main_clusters,
            Position = Position.ShrinkB(50.0f).SliceB(40.0f).ShrinkX(25.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
            .Conditional(show_more_info.Get >> not)

        |+ Button(
            (fun () -> sprintf "MA: %s  •  PA: %s" (!stats).MA (!stats).PA),
            (fun () -> show_more_info.Set false),
            Position = Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f),
            TextColor = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text
        )
            .Conditional(show_more_info.Get)
        |+ Text(
            (fun () -> sprintf "Taps ~ M: %.2fms  •  SD: %.2fms" (!stats).TapMean (!stats).TapStandardDeviation),
            Position = Position.ShrinkT(640.0f).SliceT(40.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER,
            Color = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text
        )
            .Conditional(show_more_info.Get)
        |+ Text(
            (fun () -> sprintf "%.1fms earliest  •  +%.1fms latest  •  %.1f%% early" (fst (!stats).TapRange) (snd (!stats).TapRange) (100.0 * (!stats).TapEarlyPercent)),
            Position = Position.ShrinkT(675.0f).SliceT(40.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER,
            Color = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text_subheading
        )
            .Conditional(show_more_info.Get)
        |+ Text(
            (fun () -> sprintf "Releases ~ M: %.2fms  •  SD: %.2fms" (!stats).ReleaseMean (!stats).ReleaseStandardDeviation),
            Position = Position.ShrinkT(715.0f).SliceT(40.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER,
            Color = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text
        )
            .Conditional(show_more_info.Get)
        |* Text(
            (fun () -> sprintf "%.1fms earliest  •  +%.1fms latest  •  %.1f%% early" (fst (!stats).ReleaseRange) (snd (!stats).ReleaseRange) (100.0 * (!stats).ReleaseEarlyPercent)),
            Position = Position.ShrinkT(750.0f).SliceT(40.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER,
            Color = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text_subheading
        )
            .Conditional(show_more_info.Get)

        base.Init(parent)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, Colors.white, 2.0f)
        Render.rect_c this.Bounds (Quad.gradient_top_to_bottom (!*Palette.DARKER.O3) Colors.shadow_2.O3)
        base.Draw()

        // accuracy info
        let counters = Rect.Box(this.Bounds.Left + 25.0f, this.Bounds.Top + 160.0f + 10.0f, this.Bounds.Width - 50.0f, 350.0f)

        let judgement_counts = (!stats).Judgements
        let judgements = score_info.Ruleset.Judgements |> Array.indexed
        let h = counters.Height / float32 judgements.Length
        let mutable y = counters.Top

        for i, j in judgements do
            let b = Rect.Create(counters.Left, y, counters.Right, y + h)
            Render.rect b j.Color.O1

            Render.rect
                (b.SliceL(
                    counters.Width
                    * (float32 judgement_counts.[i] / float32 (!stats).JudgementCount)
                ))
                j.Color.O2

            Text.fill_b (
                Style.font,
                sprintf "%s: %i" j.Name judgement_counts.[i],
                b.Shrink(10.0f, 2.0f),
                (if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.LEFT
            )

            y <- y + h