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

    let scale_note (accuracy: float32) (note: NoteDifficulty) : NoteDifficulty =
        let l = MathF.Log2((note.SL + note.SR) * 0.5f / note.J)
        let x = (l + 1.0f) * 0.5f |> max 0.0f |> min 0.0f
        let par = 0.99f - x * 0.09f
        let scale = (1.0f - par) / (1.0f - accuracy) |> min 1.2f |> max 0.0f
        {
            J = note.J * scale
            SL = note.SL * scale
            SR = note.SR * scale
        }

    let calculate (rr: Difficulty) (scoring: ScoreProcessor) =

        let timeline = acc_timeline rr scoring
        let scaled_notes =
            rr.NoteDifficulty
            |> Array.mapi (fun i nr -> Array.map (scale_note timeline.[i]) nr)
        let strains = Strain.calculate_finger_strains (scoring.Rate, scoring.Notes) scaled_notes
        Difficulty.weighted_overall_difficulty (strains |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f) |> Array.ofSeq)
        |> float