namespace Prelude.Gameplay

open System;
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score

(*
    Representation of hand/keyboard layouts.
    Used in the difficulty calculation to model how some things strain your hand(s) differently depending on which fingers are used
*)

type Layout = Spread | OneHand | LeftOne | RightOne | LeftTwo | RightTwo | BMSLeft | BMSRight
type Hand = int list

module Layout = 

    let getFingerPosition k h =
        let rec f k h i =
            match h with 
            | n :: fs ->
                if n = k then i else f k fs (i+1)
            | [] -> -1
        f k h 0

    let getHandBitMask h = h |> Seq.ofList |> makeBitmap

    //Expected to only contain up to 2 elements but this allows mappings
    type LayoutInfo = Hand list

    let getLayoutInfo(l, keycount): LayoutInfo option =
        match (l, keycount) with
        | (OneHand, 3) -> Some [[0;1;2]]
        | (LeftOne, 3) -> Some [[0;1];[2]]
        | (RightOne, 3) -> Some [[0];[1;2]]

        | (OneHand, 4) -> Some [[0;1;2;3]]
        | (Spread, 4) -> Some [[0;1];[2;3]]
        | (LeftOne, 4) -> Some [[0;1;2];[3]]
        | (RightOne, 4) -> Some [[0];[1;2;3]]

        | (OneHand, 5) -> Some [[0;1;2;3;4]]
        | (LeftOne, 5) -> Some [[0;1;2];[3;4]]
        | (RightOne, 5) -> Some [[0;1];[2;3;4]]
        | (LeftTwo, 5) -> Some [[0;1;2;3];[4]]
        | (RightTwo, 5) -> Some [[0];[1;2;3;4]]

        | (Spread, 6) -> Some [[0;1;2];[3;4;5]]
        | (LeftOne, 6) -> Some [[0;1;2;3];[4;5]]
        | (RightOne, 6) -> Some [[0;1];[2;3;4;5]]
        | (LeftTwo, 6) -> Some [[0;1;2;3;4];[5]]
        | (RightTwo, 6) -> Some [[0];[1;2;3;4;5]]

        | (LeftOne, 7) -> Some [[0;1;2;3];[4;5;6]]
        | (RightOne, 7) -> Some [[0;1;2];[3;4;5;6]]
        | (LeftTwo, 7) -> Some [[0;1;2;3;4];[5;6]]
        | (RightTwo, 7) -> Some [[0;1];[2;3;4;5;6]]
        | (BMSLeft, 7) -> Some [[0;1;3;2];[4;5;6]]
        | (BMSRight, 7) -> Some [[0;1;2];[4;3;5;6]]

        | (Spread, 8) -> Some [[0;1;2;3];[4;5;6;7]]
        | (LeftOne, 8) -> Some [[0;1;2];[3;4;5;6;7]]
        | (RightOne, 8) -> Some [[0;1;2;3;4];[5;6;7]]

        | (LeftOne, 9) -> Some [[0;1;2;3;4];[5;6;7;8]]
        | (RightOne, 9) -> Some [[0;1;2;3];[4;5;6;7;8]]

        | (Spread, 10) -> Some [[0;1;2;3;4];[5;6;7;8;9]]

        | _ -> None

    let getLayoutName(l, keycount) =
        match (l, keycount) with
        | (OneHand, 3) -> "One-Handed"
        | (LeftOne, 3) -> "2k+1"
        | (RightOne, 3) -> "1k+2"
    
        | (OneHand, 4) -> "One-Handed"
        | (Spread, 4) -> "Spread"
        | (LeftOne, 4) -> "3k+1"
        | (RightOne, 4) -> "1k+3"
    
        | (OneHand, 5) -> "One-Handed"
        | (LeftOne, 5) -> "3k+2"
        | (RightOne, 5) -> "2k+3"
        | (LeftTwo, 5) -> "4k+1"
        | (RightTwo, 5) -> "1k+4"
    
        | (Spread, 6) -> "Spread"
        | (LeftOne, 6) -> "4k+2"
        | (RightOne, 6) -> "2k+4"
        | (LeftTwo, 6) -> "5k+1"
        | (RightTwo, 6) -> "1k+5"
    
        | (LeftOne, 7) -> "Left Thumb"
        | (RightOne, 7) -> "Right Thumb"
        | (LeftTwo, 7) -> "5k+2"
        | (RightTwo, 7) -> "2k+5"
        | (BMSLeft, 7) -> "IIDX Left Thumb"
        | (BMSRight, 7) -> "IIDX Right Thumb"
    
        | (Spread, 8) -> "Spread"
        | (LeftOne, 8) -> "5k+3"
        | (RightOne, 8) -> "3k+5"

        | (LeftOne, 9) -> "Left Thumb"
        | (RightOne, 9) -> "Right Thumb"

        | (Spread, 10) -> "Spread"

        | _ -> "Unknown Layout"

    let getAvailableLayouts (k: int) = 
        [(Spread, k); (OneHand, k); (LeftOne, k); (RightOne, k);
            (LeftTwo, k); (RightTwo, k); (BMSLeft, k); (BMSRight, k)]
        |> List.filter (getLayoutInfo >> Option.isSome)
        |> List.map (fun (a, b) -> a)


module Difficulty =

    open Layout
(*
    Difficulty calculation
    this is all old and bad and i will be rewriting it properly - this is just a port of the C# version
*)

    let private jackCurve delta =
        let widthScale = 0.02
        let heightScale = 26.5
        let curveExp = 1.0
        Math.Min(heightScale / Math.Pow(widthScale * delta, curveExp), 20.0)

    let private streamCurve delta =
        let widthScale = 0.02
        let heightScale = 13.5
        let curveExp = 1.0
        let cutoff = 10.0
        Math.Max((heightScale / Math.Pow(widthScale * delta, curveExp) - 0.1 * heightScale / Math.Pow(widthScale * delta, curveExp * cutoff)), 0.0)

    let private jackCompensation jackDelta streamDelta =
        Math.Min(Math.Pow(Math.Max(Math.Log(jackDelta/streamDelta, 2.0), 0.0), 2.0), 1.0)

    let private rootMeanPower values power = 
        match values with
            | x :: [] -> x
            | [] -> 0.0
            | xs ->
                let (count, sumpow) = List.fold (fun (count,a) b -> (count + 1.0, a + Math.Pow(b, power))) (0.0, 0.0) xs
                Math.Pow(sumpow / count, 1.0 / power)

    let private staminaFunc value input delta =
        let staminaBaseFunc(ratio) = 1.0 + 0.105 * ratio
        let staminaDecayFunc(delta) = Math.Exp(-0.00045 * delta)
        let v = Math.Max(value * staminaDecayFunc(delta), 0.001)
        v * staminaBaseFunc(input/v)
   
    let private overallDifficulty arr =
        Math.Pow(Array.fold (fun v x -> v * Math.Exp(0.01 * Math.Max(0.0, Math.Log(x/v)))) 0.01 arr, 0.6) * 2.5

    let private OHTNERF = 3.0
    let private SCALING_VALUE = 0.55

    type RatingReport(notes: TimeData<NoteRow>, rate: float, layout, keys) =
        let layoutData =
            match getLayoutInfo(layout, keys) with
            | Some l -> l
            | None -> getAvailableLayouts keys |> List.head |> fun l -> getLayoutInfo(l, keys) |> extractOption
        let fingers = Array.zeroCreate keys

        let physicalData = Array.zeroCreate notes.Count
        let technicalData = Array.zeroCreate notes.Count

        let delta = Array2D.zeroCreate notes.Count keys
        let jack = Array2D.zeroCreate notes.Count keys
        let trill = Array2D.zeroCreate notes.Count keys
        let anchor = Array2D.zeroCreate notes.Count keys
        let physicalComposite = Array2D.zeroCreate notes.Count keys

        let updateNoteDifficulty(column, index, offset : float, otherColumns : Bitmap) =
            let s = otherColumns |> unsetBit column
            let delta1 =
                if fingers.[column] > 0.0 then
                    delta.[index, column] <- (offset - fingers.[column]) / rate
                    jack.[index,column] <- Math.Pow(jackCurve(delta.[index,column]), OHTNERF)
                    delta.[index, column]
                else 10000.0
            for k in getBits s do
                if fingers.[k] > 0.0 then
                    let delta2 = (offset - fingers.[k]) / rate
                    trill.[index, column] <- trill.[index, column] + Math.Pow(streamCurve delta2 * (jackCompensation delta1 delta2), OHTNERF)
            physicalComposite.[index, column] <- Math.Pow(trill.[index,column] + jack.[index, column], 1.0 / OHTNERF)

        let snapDifficulty (strain : float array) mask =
            let mutable vals = []
            for k in getBits mask do
                vals <- strain.[k] :: vals
            rootMeanPower vals 1.0

        let (physical, technical) =
            let lastHandUse = Array.zeroCreate (List.length layoutData)
            let currentStrain = Array.zeroCreate<float> keys
            let mutable i = 0
            for (offset, nr) in notes.Enumerate do
                let hits = (noteData NoteType.NORMAL nr) ||| (noteData NoteType.HOLDHEAD nr)
                if hits > 0us then
                    List.fold (fun h hand ->
                        let handHits = hits &&& getHandBitMask hand
                        let delta = (offset - lastHandUse.[h]) / rate
                        for k in getBits handHits do
                            updateNoteDifficulty(k, i, offset, getHandBitMask hand)
                            currentStrain.[k] <- staminaFunc (currentStrain.[k]) (physicalComposite.[i,k] * SCALING_VALUE) ((offset - fingers.[k]) / rate)
                        for k in getBits handHits do
                            fingers.[k] <- offset
                        lastHandUse.[h] <- offset
                        h + 1) 0 layoutData |> ignore
                    physicalData.[i] <- snapDifficulty currentStrain hits
                    technicalData.[i] <- countBits hits |> float
                i <- i + 1
            (overallDifficulty physicalData, overallDifficulty technicalData)

        member this.Physical = physical
        member this.Technical = technical

        member this.PhysicalData = physicalData
        member this.TechnicalData = technicalData

        member this.PhysicalComposite = physicalComposite

    let private confidenceValue (delta : float) =
        let phi x =
            let y = x / 1.414213562
            Math.Max(0.0, Math.Min((0.5 + Math.Pow(Math.PI, -0.5) * (y - Math.Pow(y, 3.0) / 3.0 + Math.Pow(y, 5.0)/ 10.0 - Math.Pow(y, 7.0) / 42.0 + Math.Pow(y, 9.0)/ 216.0)), 1.0))
        phi((17.95 - Math.Max(2.0, Math.Abs(delta))) / 15.0)

    let performanceFunc b value deviation delta =
        staminaFunc b (value * confidenceValue deviation) delta

    type PerformanceMetricState = float * float * float array * float * float array
    let performanceMetric (rr : RatingReport) (keys: int) =
        ScoreMetric<PerformanceMetricState>(
            "Interlude",
            (0.0, 0.01, Array.zeroCreate keys, 0.01, Array.zeroCreate keys),
            (fun (time, dev, hit) i (lastTime, p, ps, t, ts) ->
                let mutable v = 0.0
                let mutable c = 0.0
                for k = 0 to (keys - 1) do
                    if hit.[k] = HitStatus.Hit then
                        ps.[k] <- performanceFunc (ps.[k]) (rr.PhysicalComposite.[i, k]) (dev.[k]) (time-lastTime)
                        c <- c + 1.0
                        v <- v + ps.[k]
                (time, p * Math.Exp(0.01 * Math.Max(0.0, Math.Log(v/c/p))), ps, t, ts)
            ),
            (fun x y -> id),
            fun (_, p, ps, t, ts) -> Math.Pow(p, 0.6) * 2.5)