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

    let hold_mechanics_type =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania _) -> 0
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr _) -> 1
        | HoldMechanics.OnlyRequireHold _ -> 2
        | HoldMechanics.JudgeReleasesSeparately _ -> 3
        | HoldMechanics.OnlyJudgeReleases _ -> 4
        |> Setting.simple

    let release_early_window =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (early, _, _, _)) -> early
        | _ -> fst ruleset.Value.NoteWindows
        |> Setting.bounded (-500.0f<ms / rate>, 0.0f<ms / rate>)

    let release_late_window =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (_, late, _, _)) -> late
        | _ -> snd ruleset.Value.NoteWindows
        |> Setting.bounded (0.0f<ms / rate>, 500.0f<ms / rate>)

    let release_window =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.OnlyRequireHold release_window -> release_window
        | _ -> snd ruleset.Value.NoteWindows
        |> Setting.bounded (0.0f<ms / rate>, 500.0f<ms / rate>)

    let judgement_if_dropped =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (_, _, if_dropped, _)) -> if_dropped
        | HoldMechanics.OnlyJudgeReleases j -> j
        | _ -> ruleset.Value.DefaultJudgement
        |> Setting.simple

    let judgement_if_overheld =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (_, _, _, if_overheld)) -> if_overheld
        | HoldMechanics.JudgeReleasesSeparately (_, j) -> j
        | _ -> ruleset.Value.DefaultJudgement
        |> Setting.simple

    let release_timing_windows =
        match ruleset.Value.HoldMechanics with
        | HoldMechanics.JudgeReleasesSeparately (w, _) -> w
        | _ -> ruleset.Value.Judgements |> Array.map (_.TimingWindows)

    override this.Content() =
        let ghost_tap_judgement_options : (int option * string) array =
            Array.append
                [| None, %"rulesets.mechanics.ghost_tap_judgement.none" |]
                (ruleset.Value.Judgements |> Array.indexed |> Array.map (fun (i, j) -> Some i, j.Name))

        let judgement_dropdown(setting: Setting<int>) =
            SelectDropdown(ruleset.Value.Judgements |> Array.map _.Name |> Array.indexed, setting)

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
        |+ PageSetting(%"rulesets.mechanics.cbrush_window", NumberEntry.create_uom "ms" cbrush_window)
            .Help(Help.Info("rulesets.mechanics.cbrush_window"))
            .Conditional(fun () -> note_priority.Value = 0)
            .Pos(4)
        |+ PageSetting(%"rulesets.mechanics.hold_mechanics",
            SelectDropdown(
                if hold_mechanics_type.Value = 0 then
                    [| 0, %"rulesets.mechanics.osu_mania" |]
                else
                    [|
                        1, %"rulesets.mechanics.head_judgement_or"
                        2, %"rulesets.mechanics.just_require_hold"
                        3, %"rulesets.mechanics.judge_releases_separately"
                        4, %"rulesets.mechanics.judge_releases_only"
                    |]
                , hold_mechanics_type
            )
        )
            .Pos(7)

        |+ Text(%"rulesets.mechanics.cannot_edit", Align = Alignment.LEFT, Color = K Colors.text_subheading).Position(Position.Shrink(Style.PADDING))
            .Conditional(fun () -> hold_mechanics_type.Value = 0)
            .Pos(9)

        |+ PageSetting(%"rulesets.mechanics.early_release_window", NumberEntry.create_uom "ms" release_early_window)
            .Help(Help.Info("rulesets.mechanics.early_release_window"))
            .Conditional(fun () -> hold_mechanics_type.Value = 1)
            .Pos(9)
        |+ PageSetting(%"rulesets.mechanics.late_release_window", NumberEntry.create_uom "ms" release_late_window)
            .Help(Help.Info("rulesets.mechanics.late_release_window"))
            .Conditional(fun () -> hold_mechanics_type.Value = 1)
            .Pos(11)
        |+ PageSetting(%"rulesets.mechanics.judgement_if_dropped", judgement_dropdown judgement_if_dropped)
            .Help(Help.Info("rulesets.mechanics.judgement_if_dropped"))
            .Conditional(fun () -> hold_mechanics_type.Value = 1)
            .Pos(13)
        |+ PageSetting(%"rulesets.mechanics.judgement_if_overheld", judgement_dropdown judgement_if_overheld)
            .Help(Help.Info("rulesets.mechanics.judgement_if_overheld"))
            .Conditional(fun () -> hold_mechanics_type.Value = 1)
            .Pos(15)
        |+ (
            Callout.frame
                (Callout.Normal.Icon(Icons.INFO).Title(%"rulesets.mechanics.head_judgement_or").Body(%"rulesets.mechanics.head_judgement_or.desc"))
                (fun (w, h) -> page_position(18, 5, PageWidth.Custom w))
        )
            .Conditional(fun () -> hold_mechanics_type.Value = 1)

        |+ PageSetting(%"rulesets.mechanics.release_window", NumberEntry.create_uom "ms" release_window)
            .Help(Help.Info("rulesets.mechanics.release_window"))
            .Conditional(fun () -> hold_mechanics_type.Value = 2)
            .Pos(9)
        |+ (
            Callout.frame
                (Callout.Normal.Icon(Icons.INFO).Title(%"rulesets.mechanics.just_require_hold").Body(%"rulesets.mechanics.just_require_hold.desc"))
                (fun (w, h) -> page_position(12, 5, PageWidth.Custom w))
        )
            .Conditional(fun () -> hold_mechanics_type.Value = 2)

        |+ PageSetting(%"rulesets.mechanics.judgement_if_overheld", judgement_dropdown judgement_if_overheld)
            .Help(Help.Info("rulesets.mechanics.judgement_if_overheld"))
            .Conditional(fun () -> hold_mechanics_type.Value = 3)
            .Pos(9)
        |+ PageButton(%"rulesets.mechanics.release_windows", fun () -> EditWindows.release_windows(ruleset.Value.Judgements, release_timing_windows).Show())
            .Conditional(fun () -> hold_mechanics_type.Value = 3)
            .Pos(11)
        |+ (
            Callout.frame
                (Callout.Normal.Icon(Icons.INFO).Title(%"rulesets.mechanics.judge_releases_separately").Body(%"rulesets.mechanics.judge_releases_separately.desc"))
                (fun (w, h) -> page_position(14, 6, PageWidth.Custom w))
        )
            .Conditional(fun () -> hold_mechanics_type.Value = 3)

        |+ PageSetting(%"rulesets.mechanics.judgement_if_dropped", judgement_dropdown judgement_if_dropped)
            .Help(Help.Info("rulesets.mechanics.judgement_if_dropped"))
            .Conditional(fun () -> hold_mechanics_type.Value = 4)
            .Pos(9)
        |+ PageButton(%"rulesets.mechanics.release_windows", fun () -> EditWindows.notes_windows_as_release_windows(ruleset).Show())
            .Conditional(fun () -> hold_mechanics_type.Value = 4)
            .Pos(11)
        |+ (
            Callout.frame
                (Callout.Normal.Icon(Icons.INFO).Title(%"rulesets.mechanics.judge_releases_only").Body(%"rulesets.mechanics.judge_releases_only.desc"))
                (fun (w, h) -> page_position(14, 5, PageWidth.Custom w))
        )
            .Conditional(fun () -> hold_mechanics_type.Value = 4)

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
                HoldMechanics =
                    match hold_mechanics_type.Value with
                    | 1 ->
                        HoldMechanics.CombineHeadAndTail (
                            HeadTailCombineRule.HeadJudgementOr (
                                release_early_window.Value, release_late_window.Value,
                                judgement_if_dropped.Value, judgement_if_overheld.Value
                            )
                        )
                    | 2 -> HoldMechanics.OnlyRequireHold release_window.Value
                    | 3 -> HoldMechanics.JudgeReleasesSeparately (release_timing_windows, judgement_if_overheld.Value)
                    | 4 -> HoldMechanics.OnlyJudgeReleases (judgement_if_dropped.Value)
                    | _ -> ruleset.Value.HoldMechanics
            }