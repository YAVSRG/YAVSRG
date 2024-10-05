namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.UI

#nowarn "3370"

type Sidebar(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let show_more_info = Setting.simple false

    let mod_string = Mods.format (score_info.Rate, score_info.Mods, false)

    let category, main_elements, minor_elements =
        let c =  score_info.ChartMeta.Patterns.Category
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
            (fun () -> sprintf "MA: %s  •  PA: %s  •  M: %.1fms  •  SD: %.1fms" (!stats).MA (!stats).PA (!stats).TapMean (!stats).TapStandardDeviation),
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
            main_elements,
            Position = Position.ShrinkB(50.0f).SliceB(40.0f).ShrinkX(25.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
            .Conditional(show_more_info.Get >> not)
        |+ Text(
            minor_elements,
            Position = Position.ShrinkB(20.0f).SliceB(30.0f).ShrinkX(25.0f),
            Color = K Colors.text_greyout,
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
            (fun () -> sprintf "Taps ~ M: %.1fms  •  SD: %.1fms" (!stats).TapMean (!stats).TapStandardDeviation),
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
            (fun () -> sprintf "Releases ~ M: %.1fms  •  SD: %.1fms" (!stats).ReleaseMean (!stats).ReleaseStandardDeviation),
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
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, Colors.white, 2.0f)
        Draw.untextured_quad this.Bounds.AsQuad (Quad.gradient_top_to_bottom (!*Palette.DARKER.O3) Colors.shadow_2.O3)
        base.Draw()

        // accuracy info
        let counters = Rect.Box(this.Bounds.Left + 25.0f, this.Bounds.Top + 160.0f + 10.0f, this.Bounds.Width - 50.0f, 350.0f)

        let judgement_counts = (!stats).Judgements
        let judgements = score_info.Ruleset.Judgements |> Array.indexed
        let h = counters.Height / float32 judgements.Length
        let mutable y = counters.Top

        for i, j in judgements do
            let b = Rect.Create(counters.Left, y, counters.Right, y + h)
            Draw.rect b j.Color.O1

            Draw.rect
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