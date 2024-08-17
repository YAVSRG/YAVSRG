namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Data
open Interlude.UI

#nowarn "3370"

type Sidebar(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let mod_string = Mods.format (score_info.Rate, score_info.Mods, false)

    let category, main_elements, minor_elements =
        let c =  score_info.Patterns.Category
        c.Category,
        String.concat ", " c.MajorFeatures |> (function "" -> "--" | x -> x),
        String.concat ", " c.MinorFeatures |> (function "" -> "--" | x -> x)

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
            sprintf "%s %.2f" Icons.STAR score_info.Rating.Physical,
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            (fun () -> sprintf "%ix" score_info.Scoring.State.BestCombo),
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER
        )
        |+ Text(
            sprintf "%.2f" score_info.Physical,
            Position = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f),
            Align = Alignment.RIGHT
        )
        // todo: update this on ruleset change
        |+ Help(
            Callout.Normal
                .Icon(Icons.TRENDING_UP)
                .Title("Taps")
                .Body(
                    sprintf
                        "M: %.1fms | SD: %.1fms | %.1f%% early"
                        (!stats).TapMean
                        (!stats).TapStandardDeviation
                        (100.0 * (!stats).TapEarlyPercent)
                )
                .Title("Releases")
                .Body(
                    sprintf
                        "M: %.1fms | SD: %.1fms | %.1f%% early"
                        (!stats).ReleaseMean
                        (!stats).ReleaseStandardDeviation
                        (100.0 * (!stats).ReleaseEarlyPercent)
                ),
            Position = Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f)
        )
        |* Text(
            (fun () -> sprintf "MA: %s  •  PA: %s  •  M: %.1fms  •  SD: %.1fms" (!stats).MA (!stats).PA (!stats).TapMean (!stats).TapStandardDeviation),
            Position = Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f),
            Align = Alignment.CENTER,
            Color = fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text
        )

        this
        |+ Text(
            category,
            Position = Position.ShrinkB(95.0f).SliceB(60.0f).ShrinkX(25.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            main_elements,
            Position = Position.ShrinkB(60.0f).SliceB(40.0f).ShrinkX(25.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |* Text(
            minor_elements,
            Position = Position.ShrinkB(30.0f).SliceB(30.0f).ShrinkX(25.0f),
            Color = K Colors.text_greyout,
            Align = Alignment.LEFT
        )

        base.Init(parent)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds.SliceT(160.0f), !*Palette.DARKER, 2.0f)
        Background.draw (this.Bounds.ShrinkT(160.0f), (Color.FromArgb(40, 40, 40)), 2.0f)
        base.Draw()

        // accuracy info
        let counters =
            Rect.Box(this.Bounds.Left + 25.0f, this.Bounds.Top + 160.0f + 10.0f, this.Bounds.Width - 50.0f, 350.0f)

        let judgement_counts = (!stats).Judgements
        let judgements = score_info.Ruleset.Judgements |> Array.indexed
        let h = counters.Height / float32 judgements.Length
        let mutable y = counters.Top

        for i, j in judgements do
            let b = Rect.Create(counters.Left, y, counters.Right, y + h)
            Draw.rect b (Color.FromArgb(40, j.Color))

            Draw.rect
                (b.SliceL(
                    counters.Width
                    * (float32 judgement_counts.[i] / float32 (!stats).JudgementCount)
                ))
                (Color.FromArgb(127, j.Color))

            Text.fill_b (
                Style.font,
                sprintf "%s: %i" j.Name judgement_counts.[i],
                b.Shrink(15.0f, 2.0f),
                (if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.LEFT
            )

            y <- y + h