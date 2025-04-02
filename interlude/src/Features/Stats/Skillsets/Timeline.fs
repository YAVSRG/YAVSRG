namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Calculator
open Prelude.Data.User.Stats
open Interlude.UI
open Interlude.Features.Gameplay

type SkillTimeline() =
    inherit Container(NodeType.None)

    let graph_container =
        SwapContainer()
            .Position(Position.ShrinkT(50.0f))

    let day_range = Animation.Fade(90.0f)
    let day_offset = Animation.Fade(0.0f)

    let default_keymode = SelectedChart.keymode() |> int
    let keymode = Setting.simple default_keymode

    let refresh_graph() =
        graph_container.Current <- SkillTimelineGraph(keymode.Value, day_range, day_offset)

    override this.Init(parent: Widget) =
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default || i = default_keymode then
                        yield i
            }
            |> Array.ofSeq

        refresh_graph()

        let keymode_switcher =
            AngledButton(
                (fun () -> sprintf "%iK" keymode.Value),
                (fun () ->
                    keymode.Value <- available_keymodes.[(1 + Array.findIndex ((=) keymode.Value) available_keymodes) % available_keymodes.Length]
                    refresh_graph()
                ),
                Colors.shadow_2.O2
            )
                .LeanRight(false)
                .Position(Position.SliceT(AngledButton.HEIGHT).SliceR(150.0f))

        let zoom_in =
            AngledButton(
                Icons.ZOOM_IN,
                (fun () -> day_range.Target <- max 30.0f (day_range.Target - 30.0f)),
                Colors.black.O2
            )
                .Hotkey("uprate")
                .Position(Position.SliceT(AngledButton.HEIGHT).ShrinkR(175.0f).SliceR(100.0f))

        let zoom_out =
            AngledButton(
                Icons.ZOOM_OUT,
                (fun () -> day_range.Target <- min 390.0f (day_range.Target + 30.0f)),
                Colors.shadow_2.O2
            )
                .Hotkey("downrate")
                .Position(Position.SliceT(AngledButton.HEIGHT).ShrinkR(300.0f).SliceR(100.0f))

        let show_newer =
            AngledButton(
                Icons.ARROW_RIGHT,
                (fun () -> day_offset.Target <- max 0.0f (day_offset.Target - day_range.Target * 0.25f)),
                Colors.black.O2
            )
                .Position(Position.SliceT(AngledButton.HEIGHT).ShrinkR(425.0f).SliceR(100.0f))

        let show_older =
            AngledButton(
                Icons.ARROW_LEFT,
                (fun () -> day_offset.Target <- day_offset.Target + day_range.Target * 0.25f),
                Colors.shadow_2.O2
            )
                .Position(Position.SliceT(AngledButton.HEIGHT).ShrinkR(550.0f).SliceR(100.0f))

        this
            .Add(
                keymode_switcher,
                zoom_in,
                zoom_out,
                show_newer,
                show_older,
                graph_container
            )
        base.Init parent

    member this.Switch(k: int) =
        keymode.Value <- k
        refresh_graph()