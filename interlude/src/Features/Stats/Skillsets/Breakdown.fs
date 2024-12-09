namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay
open Prelude.Data.User
open Prelude.Charts.Processing.Patterns
open Interlude.UI

type SkillBreakdown() =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if Stats.TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default then
                        yield i
            }
            |> Array.ofSeq

        let available_keymodes = if available_keymodes.Length = 0 then [|4|] else available_keymodes

        let keymode = Setting.simple available_keymodes.[0]
        let skill = Setting.simple Jacks
        let source = Setting.simple AllTime

        let graph_container = SwapContainer(SkillBreakdownGraph.Create(keymode.Value, skill.Value, source.Value), Position = Position.ShrinkT(50.0f))
        let refresh_graph() =
            graph_container.Current <- SkillBreakdownGraph.Create(keymode.Value, skill.Value, source.Value)

        let source_switcher =
            StylishButton(
                (fun () ->
                    source.Value <- match source.Value with AllTime -> Recent | _ -> AllTime
                    refresh_graph()
                ),
                (fun () -> sprintf "%O" source.Value),
                K Colors.black.O2,
                TiltLeft = false,
                Position = Position.SliceT(50.0f).ShrinkR(400.0f)
            )

        let keymode_switcher =
            StylishButton(
                (fun () ->
                    keymode.Value <- available_keymodes.[(1 + Array.findIndex ((=) keymode.Value) available_keymodes) % available_keymodes.Length]
                    refresh_graph()
                ),
                (fun () -> sprintf "%iK" keymode.Value),
                K Colors.shadow_2.O2,
                Position = Position.SliceT(50.0f).ShrinkR(275.0f).SliceR(100.0f)
            )

        let skill_switcher =
            StylishButton(
                (fun () ->
                    skill.Value <- match skill.Value with Jacks -> Chordstream | Chordstream -> Stream | Stream -> Jacks
                    refresh_graph()
                ),
                (fun () -> sprintf "%O" skill.Value),
                K Colors.black.O2,
                TiltRight = false,
                Position = Position.SliceT(50.0f).SliceR(250.0f)
            )

        this
        |+ source_switcher
        |+ keymode_switcher
        |+ skill_switcher
        |* graph_container
        base.Init parent