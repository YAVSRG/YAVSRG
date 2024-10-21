namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditMechanicsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let note_priority =
        match ruleset.Value.HitMechanics.NotePriority with
        | NotePriority.Interlude _ -> 0
        | NotePriority.OsuMania -> 1
        | NotePriority.Etterna -> 2
        |> Setting.simple

    let cbrush_window = 
        let max_window = 
            let early, late = ruleset.Value.NoteWindows
            max late (abs early)

        match ruleset.Value.HitMechanics.NotePriority with
        | NotePriority.Interlude w -> w
        | _ -> max_window * 0.5f
        |> Setting.bounded (0.0f<ms / rate>, max_window)

    let ghost_tap_judgement =
        ruleset.Value.HitMechanics.GhostTapJudgement
        |> Setting.simple

    override this.Content() =
        let ghost_tap_judgement_options : (int option * string) array =
            Array.append
                [| None, %"rulesets.mechanics.ghost_tap_judgement.none" |] 
                (ruleset.Value.Judgements |> Array.indexed |> Array.map (fun (i, j) -> Some i, j.Name))

        page_container()
        |+ PageSetting(%"rulesets.mechanics.ghost_tap_judgement", 
            SelectDropdown(ghost_tap_judgement_options, ghost_tap_judgement))
            .Help(Help.Info("rulesets.mechanics.ghost_tap_judgement"))
            .Pos(0)
        |+ PageSetting(%"rulesets.mechanics.note_priority", 
            SelectDropdown(
                [| 
                    0, %"rulesets.mechanics.note_priority.interlude"
                    1, %"rulesets.mechanics.note_priority.osu_mania"
                    2, %"rulesets.mechanics.note_priority.etterna"
                |], note_priority))
            .Help(Help.Info("rulesets.mechanics.note_priority"))
            .Pos(2)
        |+ PageSetting(%"rulesets.mechanics.cbrush_window", Slider (Setting.uom cbrush_window))
            .Help(Help.Info("rulesets.mechanics.cbrush_window"))
            .Conditional(fun () -> note_priority.Value = 0)
            .Pos(4)
        |+ PageSetting(%"rulesets.mechanics.hold_mechanics", Text("Currently can't be edited", Color = K Colors.text_greyout))
            .Pos(7)
        :> Widget
        
    override this.Title = %"rulesets.edit.mechanics"
    override this.OnClose() =
        ruleset.Set 
            { ruleset.Value with 
                HitMechanics =
                    {
                        NotePriority =
                            match note_priority.Value with
                            | 2 -> NotePriority.Etterna
                            | 1 -> NotePriority.OsuMania
                            | _ -> NotePriority.Interlude cbrush_window.Value
                        GhostTapJudgement = ghost_tap_judgement.Value
                    }
            }
