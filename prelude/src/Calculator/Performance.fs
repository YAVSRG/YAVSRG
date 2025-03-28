namespace Prelude.Calculator

open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

module Performance =

    let ACC_SENSITIVITY = 0.96

    let acc_timeline (rr: Difficulty) (scoring: ScoreProcessor) : float32 array =
        let mutable v = 1.0
        let mutable i = 0
        let output : float32 array = Array.zeroCreate rr.NoteDifficulty.Length
        // todo: dropped holds have another calculation
        for ev in scoring.Events do
            while ev.Index > i do
                output.[i] <- float32 v
                i <- i + 1
            match ev.Action.Judgement with
            | Some (_, value) ->
                v <- ACC_SENSITIVITY * v + (1.0 - ACC_SENSITIVITY) * value |> max 0.85
            | None -> ()
        output.[output.Length - 1] <- float32 v
        output

    /// Some curves put together quickly in a day to be a 'good enough' proof of concept
    /// Documentation coming soon, or if it didn't, bother me on discord for additional comment

    let tech_curve (accuracy: float32) : float32 =
        let x = 20.0f * (accuracy - 1.0f)
        let y = x / sqrt (1.0f + x * x)
        0.65f * (1.0f + y) - 0.05f |> max 0.0f

    let physical_curve (accuracy: float32) : float32 =
        let x = 9.15f - 10.0f * accuracy
        let y = x / sqrt (1.0f + x * x)
        0.5f - y |> max 0.0f

    let variety_mult (variety: float32) : float32 =
        (variety - 5.0f) / 15.0f |> min 1.0f |> max 0.0f

    let scale_note (accuracy: float32) (variety: float32) (note: NoteDifficulty) : NoteDifficulty =
        let note_mult =
            physical_curve accuracy +
            variety_mult variety * tech_curve accuracy
        {
            J = note.J * note_mult
            SL = note.SL * note_mult
            SR = note.SR * note_mult
        }

    /// Performance rating concept: Look at your accuracy throughout a score
    /// Take the accuracy and scale notes up/down in the areas you are doing good/bad in
    /// Now feed the new scaled notes through the same SR algorithm to get PR
    let calculate (rr: Difficulty) (scoring: ScoreProcessor) : float32 =

        let scoring =
            if SC_J4_HASH <> Ruleset.hash scoring.Ruleset then
                let on_standard_ruleset = scoring.Recreate(SC_J4)
                on_standard_ruleset.Update Time.infinity
                on_standard_ruleset
            else
                scoring

        let timeline = acc_timeline rr scoring
        let scaled_notes =
            rr.NoteDifficulty
            |> Array.mapi (fun i nr -> Array.map (scale_note timeline.[i] rr.Variety.[i]) nr)
        let strains = Strain.calculate_finger_strains (scoring.Rate, scoring.Notes) scaled_notes

        let note_values =
            seq {
                for i, s in Seq.indexed strains do
                    for k = 0 to scoring.Keys - 1 do
                        if rr.Strains.[i].NotesV1.[k] > 0.0f then
                            yield s.StrainV1Notes.[k]
            }

        Difficulty.weighted_overall_difficulty (note_values |> Array.ofSeq)

    let accuracy_to_rating (accuracy: float32, rate: Rate, notes: TimeArray<_>, rr: Difficulty) : float32 =
        // naive approach
        let scaled_notes = rr.NoteDifficulty |> Array.mapi (fun i nr -> Array.map (scale_note accuracy rr.Variety.[i]) nr)
        let strains = Strain.calculate_finger_strains (rate, notes) scaled_notes
        Difficulty.weighted_overall_difficulty (strains |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Array.ofSeq)