namespace Prelude.Calculator

open Prelude.Gameplay.Scoring

module Performance =

    let ACC_SENSITIVITY = 0.97

    let acc_timeline (rr: Difficulty) (scoring: ScoreProcessor) =
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
                v <- ACC_SENSITIVITY * v + (1.0 - ACC_SENSITIVITY) * value
            | None -> ()
        output.[output.Length - 1] <- float32 v
        output

    /// Some curves put together quickly in a day to be a 'good enough' proof of concept
    /// Documentation coming soon, or if it didn't, bother me on discord for additional comment

    let tech_curve (accuracy: float32) =
        let x = 20.0f * (accuracy - 1.0f)
        let y = x / sqrt (1.0f + x * x)
        0.65f * (1.0f + y) - 0.05f |> max 0.0f

    let physical_curve (accuracy: float32) =
        let x = 9.15f - 10.0f * accuracy
        let y = x / sqrt (1.0f + x * x)
        0.5f - y

    let variety_mult (variety: float32) =
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
    let calculate (rr: Difficulty) (scoring: ScoreProcessor) =

        let timeline = acc_timeline rr scoring
        let scaled_notes =
            rr.NoteDifficulty
            |> Array.mapi (fun i nr -> Array.map (scale_note timeline.[i] rr.Variety.[i]) nr)
        let strains = Strain.calculate_finger_strains (scoring.Rate, scoring.Notes) scaled_notes
        Difficulty.weighted_overall_difficulty (strains |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Array.ofSeq)
        |> float