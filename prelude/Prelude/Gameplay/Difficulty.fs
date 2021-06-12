namespace Prelude.Gameplay

open System
open System.Drawing
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score

(*
    Representation of hand/keyboard layouts.
    Used in the difficulty calculation to model how some things strain your hand(s) differently depending on which fingers are used
*)

module Layout =

    type Layout = Spread = 0 | OneHand = 1 | LeftOne = 2 | RightOne = 3 | LeftTwo = 4 | RightTwo = 5 | BMSLeft = 6 | BMSRight = 7
    type Hand = int list

    let getFingerPosition k h =
        let rec f k h i =
            match h with 
            | n :: fs ->
                if n = k then i else f k fs (i+1)
            | [] -> -1
        f k h 0

    let getHandBitMask h = h |> Seq.ofList |> Bitmap.create

    //Expected to only contain up to 2 elements but this allows mappings
    type LayoutInfo = Hand list

    let getLayoutInfo(l, keycount): LayoutInfo option =
        match (l, keycount) with
        | (Layout.OneHand, 3) -> Some [[0;1;2]]
        | (Layout.LeftOne, 3) -> Some [[0;1];[2]]
        | (Layout.RightOne, 3) -> Some [[0];[1;2]]

        | (Layout.OneHand, 4) -> Some [[0;1;2;3]]
        | (Layout.Spread, 4) -> Some [[0;1];[2;3]]
        | (Layout.LeftOne, 4) -> Some [[0;1;2];[3]]
        | (Layout.RightOne, 4) -> Some [[0];[1;2;3]]

        | (Layout.OneHand, 5) -> Some [[0;1;2;3;4]]
        | (Layout.LeftOne, 5) -> Some [[0;1;2];[3;4]]
        | (Layout.RightOne, 5) -> Some [[0;1];[2;3;4]]
        | (Layout.LeftTwo, 5) -> Some [[0;1;2;3];[4]]
        | (Layout.RightTwo, 5) -> Some [[0];[1;2;3;4]]

        | (Layout.Spread, 6) -> Some [[0;1;2];[3;4;5]]
        | (Layout.LeftOne, 6) -> Some [[0;1;2;3];[4;5]]
        | (Layout.RightOne, 6) -> Some [[0;1];[2;3;4;5]]
        | (Layout.LeftTwo, 6) -> Some [[0;1;2;3;4];[5]]
        | (Layout.RightTwo, 6) -> Some [[0];[1;2;3;4;5]]

        | (Layout.LeftOne, 7) -> Some [[0;1;2;3];[4;5;6]]
        | (Layout.RightOne, 7) -> Some [[0;1;2];[3;4;5;6]]
        | (Layout.LeftTwo, 7) -> Some [[0;1;2;3;4];[5;6]]
        | (Layout.RightTwo, 7) -> Some [[0;1];[2;3;4;5;6]]
        | (Layout.BMSLeft, 7) -> Some [[0;1;3;2];[4;5;6]]
        | (Layout.BMSRight, 7) -> Some [[0;1;2];[4;3;5;6]]

        | (Layout.Spread, 8) -> Some [[0;1;2;3];[4;5;6;7]]
        | (Layout.LeftOne, 8) -> Some [[0;1;2];[3;4;5;6;7]]
        | (Layout.RightOne, 8) -> Some [[0;1;2;3;4];[5;6;7]]

        | (Layout.LeftOne, 9) -> Some [[0;1;2;3;4];[5;6;7;8]]
        | (Layout.RightOne, 9) -> Some [[0;1;2;3];[4;5;6;7;8]]

        | (Layout.Spread, 10) -> Some [[0;1;2;3;4];[5;6;7;8;9]]

        | _ -> None

    let getLayoutName(l, keycount) =
        match (l, keycount) with
        | (Layout.OneHand, 3) -> "One-Handed"
        | (Layout.LeftOne, 3) -> "2k+1"
        | (Layout.RightOne, 3) -> "1k+2"
    
        | (Layout.OneHand, 4) -> "One-Handed"
        | (Layout.Spread, 4) -> "Spread"
        | (Layout.LeftOne, 4) -> "3k+1"
        | (Layout.RightOne, 4) -> "1k+3"
    
        | (Layout.OneHand, 5) -> "One-Handed"
        | (Layout.LeftOne, 5) -> "3k+2"
        | (Layout.RightOne, 5) -> "2k+3"
        | (Layout.LeftTwo, 5) -> "4k+1"
        | (Layout.RightTwo, 5) -> "1k+4"
    
        | (Layout.Spread, 6) -> "Spread"
        | (Layout.LeftOne, 6) -> "4k+2"
        | (Layout.RightOne, 6) -> "2k+4"
        | (Layout.LeftTwo, 6) -> "5k+1"
        | (Layout.RightTwo, 6) -> "1k+5"
    
        | (Layout.LeftOne, 7) -> "Left Thumb"
        | (Layout.RightOne, 7) -> "Right Thumb"
        | (Layout.LeftTwo, 7) -> "5k+2"
        | (Layout.RightTwo, 7) -> "2k+5"
        | (Layout.BMSLeft, 7) -> "IIDX Left Thumb"
        | (Layout.BMSRight, 7) -> "IIDX Right Thumb"
    
        | (Layout.Spread, 8) -> "Spread"
        | (Layout.LeftOne, 8) -> "5k+3"
        | (Layout.RightOne, 8) -> "3k+5"

        | (Layout.LeftOne, 9) -> "Left Thumb"
        | (Layout.RightOne, 9) -> "Right Thumb"

        | (Layout.Spread, 10) -> "Spread"

        | _ -> "Unknown Layout"

    let getAvailableLayouts (k: int) = 
        [
            (Layout.Spread, k); (Layout.OneHand, k); (Layout.LeftOne, k); (Layout.RightOne, k);
            (Layout.LeftTwo, k); (Layout.RightTwo, k); (Layout.BMSLeft, k); (Layout.BMSRight, k)
        ]
        |> List.filter (getLayoutInfo >> Option.isSome)
        |> List.map (fun (a, b) -> a)

(*
    Difficulty calculation
    this is all old and bad and i will be rewriting it properly - this is just a port of the C# version
*)

module Difficulty =
    open Layout

    let private jackCurve delta =
        let widthScale = 0.02
        let heightScale = 26.3
        let curveExp = 1.0
        Math.Min (heightScale / Math.Pow (widthScale * float delta, curveExp), 20.0)

    let private streamCurve delta =
        let widthScale = 0.02
        let heightScale = 13.7
        let curveExp = 1.0
        let cutoff = 10.0
        Math.Max ((heightScale / Math.Pow (widthScale * float delta, curveExp) - 0.1 * heightScale / Math.Pow (widthScale * float delta, curveExp * cutoff)), 0.0)

    let private jackCompensation jackDelta streamDelta =
        Math.Min (Math.Pow (Math.Max (Math.Log (float (jackDelta / streamDelta), 2.0), 0.0), 2.0), 1.0)

    let private rootMeanPower values power = 
        match values with
        | x :: [] -> x
        | [] -> 0.0
        | xs ->
            let (count, sumpow) = List.fold (fun (count, a) b -> (count + 1.0, a + Math.Pow (b, power))) (0.0, 0.0) xs
            Math.Pow (sumpow / count, 1.0 / power)

    let private staminaFunc value input delta =
        let staminaBaseFunc ratio = 1.0 + 0.105 * ratio
        let staminaDecayFunc delta = Math.Exp (-0.00044 * delta)
        let v = Math.Max (value * staminaDecayFunc (float delta), 0.01)
        v * staminaBaseFunc (input / v)
   
    let private overallDifficulty arr =
        Math.Pow (Array.fold (fun v x -> v * Math.Exp (0.01 * Math.Max (0.0, Math.Log (x / v)))) 0.01 arr, 0.6) * 2.5

    let private OHTNERF = 3.0
    let private SCALING_VALUE = 0.55

    type RatingReport(notes: TimeData<NoteRow>, rate: float32, layout, keys) =
        let layoutData =
            match getLayoutInfo(layout, keys) with
            | Some l -> l
            | None -> getAvailableLayouts keys |> List.head |> fun l -> getLayoutInfo(l, keys) |> fun x -> x.Value
        let fingers = Array.zeroCreate<Time> keys

        let physicalData = Array.zeroCreate notes.Count
        let technicalData = Array.zeroCreate notes.Count

        let delta = Array2D.zeroCreate notes.Count keys
        let jack = Array2D.zeroCreate notes.Count keys
        let trill = Array2D.zeroCreate notes.Count keys
        let anchor = Array2D.zeroCreate notes.Count keys
        let physicalComposite = Array2D.zeroCreate notes.Count keys

        let updateNoteDifficulty(column, index, offset: Time, otherColumns: Bitmap) =
            let s = otherColumns |> Bitmap.unsetBit column
            let delta1 =
                if fingers.[column] > 0.0f<ms> then
                    delta.[index, column] <- (offset - fingers.[column]) / rate
                    jack.[index,column] <- Math.Pow(jackCurve(delta.[index,column]), OHTNERF)
                    delta.[index, column]
                else 10000.0f<ms>
            for k in Bitmap.toSeq s do
                if fingers.[k] > 0.0f<ms> then
                    let delta2 = (offset - fingers.[k]) / rate
                    trill.[index, column] <- trill.[index, column] + Math.Pow((streamCurve delta2) * (jackCompensation delta1 delta2), OHTNERF)
            physicalComposite.[index, column] <- Math.Pow(trill.[index,column] + jack.[index, column], 1.0 / OHTNERF)

        let snapDifficulty (strain: float array) mask =
            //todo: optimise
            let mutable vals = []
            for k in Bitmap.toSeq mask do
                vals <- strain.[k] :: vals
            rootMeanPower vals 1.0

        let (physical, technical) =
            let lastHandUse = Array.zeroCreate (List.length layoutData)
            let currentStrain = Array.zeroCreate<float> keys
            let mutable i = 0
            for (offset, nr) in notes.Data do
                let hits = (NoteRow.noteData NoteType.NORMAL nr) ||| (NoteRow.noteData NoteType.HOLDHEAD nr)
                if hits > 0us then
                    List.fold (fun h hand ->
                        let handHits = hits &&& getHandBitMask hand
                        for k in Bitmap.toSeq handHits do
                            updateNoteDifficulty(k, i, offset, getHandBitMask hand)
                            currentStrain.[k] <- staminaFunc (currentStrain.[k]) (physicalComposite.[i,k] * SCALING_VALUE) ((offset - fingers.[k]) / rate)
                        for k in Bitmap.toSeq handHits do
                            fingers.[k] <- offset
                        lastHandUse.[h] <- offset
                        h + 1) 0 layoutData |> ignore
                    physicalData.[i] <- snapDifficulty currentStrain hits
                    technicalData.[i] <- Bitmap.count hits |> float
                i <- i + 1
            (overallDifficulty physicalData, overallDifficulty technicalData)

        member this.Physical = physical
        member this.Technical = technical

        member this.PhysicalData = physicalData
        member this.TechnicalData = technicalData

        member this.PhysicalComposite = physicalComposite

    let private confidenceValue (delta : Time) =
        let delta = float delta
        let phi x =
            let y = x / 1.414213562
            Math.Max (0.0, Math.Min ((0.5 + Math.Pow(Math.PI, -0.5) * (y - Math.Pow (y, 3.0) / 3.0 + Math.Pow (y, 5.0)/ 10.0 - Math.Pow (y, 7.0) / 42.0 + Math.Pow (y, 9.0) / 216.0)), 1.0))
        phi((17.95 - Math.Max (2.0, Math.Abs delta)) / 15.0)

    let private performanceFunc b value deviation delta =
        staminaFunc b (value * confidenceValue deviation) delta

    let technicalColor v =
        try
            let a = Math.Min(1.0, v * 0.1)
            let b = Math.Min(1.0, Math.Max(1.0, v * 0.1) - 1.0)
            Color.FromArgb (255.0 * (1.0 - a) |> int, 255.0 * b |> int, 255.0 * a |> int)
        with _ -> Color.Blue

    let physicalColor v =
        try
            let a = Math.Min(1.0, v * 0.1)
            let b = Math.Min(1.0, Math.Max(1.0, v * 0.1) - 1.0)
            Color.FromArgb(255.0 * a |> int, 255.0 * (1.0 - a) |> int, 255.0 * b |> int)
        with _ -> Color.Red

    type PerformanceMetricState = Time array * float * float array * float * float array
    let performanceMetric (rr: RatingReport) (keys: int) =
        ScoreMetric<PerformanceMetricState>(
            "Interlude",
            (Array.create keys 0.0f<ms>, 0.01, Array.zeroCreate keys, 0.01, Array.zeroCreate keys),
            (fun (time, deltas, hit) i (lastTimes, p, ps, t, ts) _ ->
                let mutable v = 0.0
                let mutable c = 0.0
                for k = 0 to (keys - 1) do
                    if hit.[k] = HitStatus.Hit then
                        ps.[k] <- performanceFunc (ps.[k]) (rr.PhysicalComposite.[i, k]) (deltas.[k]) (time - lastTimes.[k])
                        lastTimes.[k] <- time
                        c <- c + 1.0
                        v <- v + ps.[k]
                let snapRating = if c = 0.0 then 0.0 else v / c
                (lastTimes, p * Math.Exp(0.01 * Math.Max(0.0, Math.Log(snapRating / p))), ps, t, ts)
            ),
            (fun x y -> id),
            fun (_, p, ps, t, ts) -> Math.Pow(p, 0.6) * 2.5)