namespace Interlude.Features.Stats

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Calculator
open Prelude.Calculator.Patterns
open Prelude.Data.User.Stats
open Interlude.UI

type KeymodeOverview(keymode: int, button_callback: GraphSource -> CorePattern option -> unit) =
    inherit Container(NodeType.None)

    let all_time = TOTAL_STATS.KeymodeSkills.[keymode - 3].Tiny
    let recent = CURRENT_SESSION.KeymodeSkills.[keymode - 3].Tiny

    let bar (value: float32, max_value: float32, color: Color * Color) =
        { new StaticWidget(NodeType.None) with
            override this.Draw() =
                let bar = this.Bounds.ShrinkR(80.0f).Shrink(10.0f).SlicePercentL(value / max_value |> max 0.05f)
                Render.rect bar (fst color)
                Text.draw_b(Style.font, sprintf "%.0f" value, 21.0f, bar.Right + 10.0f, bar.Top - 10.0f, color)
        }

    let time_summary (data: KeymodeTinyBreakdown, source: GraphSource) =
        let total = data.Jacks + data.Chordstream + data.Stream |> max 0.1f
        let max = Array.max [| data.Jacks; data.Chordstream; data.Stream |] + 1.0f
        Container(NodeType.None)
        |+ Button(sprintf "%O: %.0f" source total, fun () -> button_callback source None)
            .Position(Position.SliceT(40.0f))

        |+ Button(Jacks.ToString(), (fun () -> button_callback source (Some Jacks)))
            .Align(Alignment.RIGHT)
            .Position(Position.SliceL(150.0f).ShrinkT(40.0f).SliceT(35.0f))
        |+ bar(data.Jacks, max, Colors.text_red)
            .Position(Position.ShrinkL(150.0f).ShrinkT(40.0f).SliceT(35.0f))

        |+ Button(Chordstream.ToString(), (fun () -> button_callback source (Some Chordstream)))
            .Align(Alignment.RIGHT)
            .Position(Position.SliceL(150.0f).ShrinkT(75.0f).SliceT(35.0f))
        |+ bar(data.Chordstream, max, Colors.text_green)
            .Position(Position.ShrinkL(150.0f).ShrinkT(75.0f).SliceT(35.0f))

        |+ Button(Stream.ToString(), (fun () -> button_callback source (Some Stream)))
            .Align(Alignment.RIGHT)
            .Position(Position.SliceL(150.0f).ShrinkT(110.0f).SliceT(35.0f))
        |+ bar(data.Stream, max, Colors.text_cyan)
            .Position(Position.ShrinkL(150.0f).ShrinkT(110.0f).SliceT(35.0f))

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

    override this.Init(parent) =
        this
        |+ time_summary(recent, Recent)
            .Position(Position.SlicePercentL(0.5f).ShrinkL(50.0f).TranslateX(50.0f))
        |+ time_summary(all_time, AllTime)
            .Position(Position.SlicePercentR(0.5f).ShrinkL(50.0f))
        |* Text(sprintf "%iK" keymode)
            .Position(Position.SliceL(100.0f).ShrinkX(20.0f).SliceY(50.0f))
        base.Init parent

type Overview(button_callback: int -> GraphSource -> CorePattern option -> unit) =
    inherit Container(NodeType.None)

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

    override this.Init(parent) =
        let container = FlowContainer.Vertical<KeymodeOverview>(150.0f, Spacing = 50.0f)
        for i = 3 to 10 do
            if TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default then
                container.Add(KeymodeOverview(i, button_callback i))
        if container.Count = 0 then
            this
            |* EmptyState(Icons.WIND, %"stats.no_data")

        this
        |* ScrollContainer(container, Margin = 50.0f)
        base.Init parent