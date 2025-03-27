namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Calculator.Patterns
open Prelude.Calculator
open Interlude.UI
open Interlude.Features.Gameplay

type Patterns(display: Setting<InfoPanelMode>) =
    inherit Container(NodeType.None)

    let mutable patterns: Cluster array = [||]
    let mutable category: string = ""

    let on_chart_update(info: LoadedChartInfo) =
        patterns <- info.Patterns.Clusters |> Array.truncate 6
        category <- info.Patterns.Category

    override this.Init(parent: Widget) =
        base.Init parent
        SelectedChart.on_chart_change_finished.Add on_chart_update
        SelectedChart.when_loaded false on_chart_update

        this
        |* AngledButton(
            %"levelselect.info.details",
            (fun () -> display.Set InfoPanelMode.Local),
            Palette.MAIN_100
        )
            .Hotkey("scoreboard_storage")
            .LeanLeft(false)
            .LeanRight(false)
            .Position(Position.SliceT(AngledButton.HEIGHT))
            .Help(Help.Info("levelselect.info.mode", "scoreboard_storage"))

    override this.Draw() =
        base.Draw()

        let mutable b =
            this.Bounds.SliceT(60.0f).Shrink(20.0f, 0.0f).Translate(0.0f, 60.0f)

        let TEXT_WIDTH = 240.0f
        let BAR_L = b.Left + TEXT_WIDTH + 5.0f
        let BAR_R = b.Right - 5.0f
        let BAR_WIDTH = BAR_R - BAR_L

        for entry in patterns do
            Text.fill_b (
                Style.font,
                (sprintf "%s%O" (if entry.Mixed then "Mixed " else "") entry.Pattern),
                b.ShrinkB(25.0f).SliceL(TEXT_WIDTH),
                Colors.text,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                (
                    if entry.Mixed then
                        sprintf "~%.0f BPM / %.2f*" (float32 entry.BPM * SelectedChart.rate.Value) entry.Rating
                    else
                        sprintf "%.0f BPM / %.2f*" (float32 entry.BPM * SelectedChart.rate.Value) entry.Rating
                ),
                b.SliceB(30.0f).SliceL(TEXT_WIDTH),
                Colors.text_subheading,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                String.concat ", " (entry.SpecificTypes |> Seq.truncate 3 |> Seq.map (fun (p, amount) -> sprintf "%.0f%% %s" (amount * 100.0f) p)),
                b.SliceB(30.0f).ShrinkL(TEXT_WIDTH),
                Colors.text_subheading,
                Alignment.LEFT
            )

            Render.rect (b.SliceL(5.0f).SliceT(20.0f).Translate(TEXT_WIDTH, 10.0f)) Colors.white
            Render.rect (b.SliceR(5.0f).SliceT(20.0f).Translate(0.0f, 10.0f)) Colors.white

            let density_color (nps: float32</rate>) =
                nps * 2.0f * SelectedChart.rate.Value |> Difficulty.color

            let bar_scale = min 1.0f (entry.Amount / 1000.0f<ms / rate> / SelectedChart.rate.Value / 100.0f)

            let bar (lo_pc, lo_val, hi_pc, hi_val) =
                Render.rect_edges_c
                    (BAR_L + lo_pc * BAR_WIDTH * bar_scale)
                    (b.Top + 12.5f)
                    (BAR_L + hi_pc * BAR_WIDTH * bar_scale)
                    (b.Top + 27.5f)
                    (Quad.gradient_left_to_right (density_color lo_val) (density_color hi_val))

            bar (0.0f, entry.Density10, 0.1f, entry.Density10)
            bar (0.1f, entry.Density10, 0.25f, entry.Density25)
            bar (0.25f, entry.Density25, 0.5f, entry.Density50)
            bar (0.5f, entry.Density50, 0.75f, entry.Density75)
            bar (0.75f, entry.Density75, 0.9f, entry.Density90)
            bar (0.9f, entry.Density90, 1.0f, entry.Density90)

            b <- b.Translate(0.0f, 60.0f)