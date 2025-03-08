namespace Prelude.Calculator

open System
open Prelude.Gameplay.Scoring

module Performance =

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
                v <- 0.95 * v + 0.05 * value
            | None -> ()
        output.[output.Length - 1] <- float32 v
        output

    let tech_curve (accuracy: float32) =
        let x = 20.0f * (accuracy - 1.0f)
        let y = x / sqrt (1.0f + x * x)
        0.85f * (1.0f + y) - 0.05f |> max 0.0f

    let physical_curve (accuracy: float32) =
        let x = 9.25f - 10.0f * accuracy
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

    let calculate (rr: Difficulty) (scoring: ScoreProcessor) =

        let timeline = acc_timeline rr scoring
        let scaled_notes =
            rr.NoteDifficulty
            |> Array.mapi (fun i nr -> Array.map (scale_note timeline.[i] rr.Variety.[i]) nr)
        let strains = Strain.calculate_finger_strains (scoring.Rate, scoring.Notes) scaled_notes
        Difficulty.weighted_overall_difficulty (strains |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Array.ofSeq)
        |> float