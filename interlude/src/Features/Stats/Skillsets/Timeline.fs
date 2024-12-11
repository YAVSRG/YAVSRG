namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay
open Prelude.Data.User
open Interlude.UI

type SkillTimeline() =
    inherit Container(NodeType.None)

    let graph_container = SwapContainer(Position = Position.ShrinkT(50.0f))

    let keymode = Setting.simple 4

    let refresh_graph() =
        graph_container.Current <- SkillTimelineGraph(keymode.Value)

    override this.Init(parent) =
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if Stats.TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default then
                        yield i
            }
            |> Array.ofSeq

        let available_keymodes = if available_keymodes.Length = 0 then [|4|] else available_keymodes

        keymode.Value <- available_keymodes.[0]
        refresh_graph()

        let keymode_switcher =
            StylishButton(
                (fun () ->
                    keymode.Value <- available_keymodes.[(1 + Array.findIndex ((=) keymode.Value) available_keymodes) % available_keymodes.Length]
                    refresh_graph()
                ),
                (fun () -> sprintf "%iK" keymode.Value),
                K Colors.shadow_2.O2,
                TiltRight = false,
                Position = Position.SliceT(50.0f).SliceR(100.0f)
            )

        this
        |+ keymode_switcher
        |* graph_container
        base.Init parent

    member this.Switch(k: int) =
        keymode.Value <- k
        refresh_graph()