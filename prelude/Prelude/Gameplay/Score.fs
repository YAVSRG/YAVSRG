module Prelude.Gameplay.Score

open System
open System.IO
open System.IO.Compression

(*
    Representation of replay data for a score.
    All scores are stored in this replay format and accuracy/performance is reconstructed in the client from the data.
    Replay data is just a corresponding row of hit deltas in milliseconds and tags marking things like "miss" or "hit" for each row in the chart
*)

type HitStatus =
    | Nothing = 0uy
    | NotHit = 1uy
    | Hit = 2uy
    | Special = 3uy
    | SpecialMissed = 4uy

type ScoreDataRow = float * float array * HitStatus array

type ScoreData = ScoreDataRow array

let countHits ((_, _, h): ScoreDataRow) =
    Array.fold (fun x hit ->
        if hit <> HitStatus.Nothing then x + 1 else x) 0 h

let offsetOfRow ((offset, _, _): ScoreDataRow) = offset

//todo: maybe migrate this format to something better
type Score =
    { time: DateTime
      hitdata: string
      player: string
      playerUUID: string
      rate: float
      selectedMods: unit
      layout: unit
      keycount: int }

//todo: resolve bug where decompressed score data does not have the offsets
//this will require reformatting of scores in general, might as well make a cleaner format
let decompressScoreData (sd: string) (k: int): ScoreData =
    let compressed = Convert.FromBase64String(sd)
    use outputStream = new MemoryStream()
    use inputStream = new MemoryStream(compressed)
    use gZipStream = new GZipStream(inputStream, CompressionMode.Decompress)
    gZipStream.CopyTo(outputStream)
    let raw = outputStream.ToArray()
    let result: ScoreData = Array.zeroCreate (raw.Length / (5 * k))
    for i in 0 .. (result.Length - 1) do
        let (offset, delta, hit) = (0.0, Array.zeroCreate<float32> (k), Array.zeroCreate (k))
        Array.Copy(raw, k * (i * 5), hit, 0, k)
        Buffer.BlockCopy(raw, k * (i * 5 + 1), delta, 0, k * 4)
        result.[i] <- (offset, Array.map float delta, hit)
    result

let compressScoreData (sd: ScoreData): string = failwith "nyi"

(*
    Score metrics - These are processors that run on score data and keep a running state as they go
    They can then output a value which means something depending on the metric
    For example, Wife is a score metric that gives your running % accuracy according to the rules of the Wife scoring system from Etterna

    Used for: Accuracy, Health Bar, Performance Rating
*)

type ScoreMetric<'state>(name: string, i: 'state, row_functor: ScoreDataRow -> int -> 'state -> 'state, hit_functor: ScoreDataRow -> int -> 'state -> 'state, value_functor: 'state -> float) =
    let mutable counter = 0
    let mutable state: 'state = i

    let timeSeriesData = Array.zeroCreate<float> 100

    member private this.Counter = counter

    member this.Name = name
    member this.State = state
    member this.Value = value_functor state
    member this.HandleHit column (index: int) (hitData: ScoreData) =
        state <- hit_functor (hitData.[index]) column state

    member private this.UpdateTimeSeries count =
        let i = timeSeriesData.Length * counter / count
        if (i < timeSeriesData.Length) && (timeSeriesData.[i] = 0.0) then timeSeriesData.[i] <- this.Value

    member this.Update (now: float) (hitData: ScoreData) =
        while (counter < hitData.Length) && (offsetOfRow hitData.[counter] < now) do
            state <- row_functor hitData.[counter] counter state
            this.UpdateTimeSeries hitData.Length
            counter <- counter + 1

    member this.ProcessAll(hitData: ScoreData) = this.Update infinity hitData

(*
    % Accuracy systems using score metrics
    Accuracy systems represent your performance on a score by assigning points to every note you hit
    Your % accuracy is the percent of points you score divided by the maximum points possible to score
*)

type JudgementType =
    | RIDICULOUS = 0
    | MARVELLOUS = 1
    | PERFECT = 2
    | OK = 3
    | GREAT = 4
    | GOOD = 5
    | BAD = 6
    | FUMBLE = 7
    | MISS = 8

type AccuracySystemState = int array * float * float * int * int * int

let MISSWINDOW = 180.0

let accuracy_hit_func judge_func points_func max_point_func combo_func =
    fun ((_, deltas, hit): ScoreDataRow) k ((judgementCounts, points, maxPoints, combo, maxCombo, cbs): AccuracySystemState) ->
        let j =
            match hit.[k] with
            | HitStatus.NotHit -> JudgementType.MISS
            | HitStatus.Hit -> judge_func (Math.Abs deltas.[k])
            | HitStatus.Special -> JudgementType.OK
            | HitStatus.SpecialMissed -> JudgementType.FUMBLE
            | _ -> failwith "impossible hit status"
        judgementCounts.[j |> int] <- (judgementCounts.[j |> int] + 1)
        let (newcombo, cb) = combo_func j combo
        (judgementCounts, points + points_func j (Math.Abs deltas.[k]), maxPoints + max_point_func j, newcombo,
         max newcombo maxCombo,
         cbs + if cb then 1 else 0)

type AccuracySystem(name: string, judge_func: float -> JudgementType, points_func: JudgementType -> float -> float, max_point_func: JudgementType -> float, combo_func: JudgementType -> int -> (int * bool)) =
    inherit ScoreMetric<AccuracySystemState>(
        name,
        ([| 0; 0; 0; 0; 0; 0; 0; 0; 0 |], 0.0, 0.0, 0, 0, 0),
         (let handle_hit =
             (accuracy_hit_func judge_func points_func max_point_func combo_func)
          (fun (offset, deltas, hit) _ (judgementCounts, points, maxPoints, combo, maxCombo, cbs) ->
              List.fold
                  (fun s (k: int) ->
                      if hit.[k] <> HitStatus.Nothing then
                          handle_hit (offset, deltas, hit) k s
                      else
                          s)
                  (judgementCounts, points, maxPoints, combo, maxCombo, cbs)
                  [ 0 .. (deltas.Length - 1) ])),
         (accuracy_hit_func judge_func points_func max_point_func combo_func),
         (fun (judgements, points, maxPoints, combo, maxCombo, cbs) -> points / maxPoints))

    member this.JudgeFunc = judge_func

type HitWindows = (float * JudgementType) list

//todo: ability to enable ridiculous timing windows
type AccuracyDisplayType =
    | Percentage
    | ProjectedScore
    | PointsScored

type AccuracySystemConfig =
    | SC of int
    | SCPlus of int
    | Wife of int
    | DP of int
    | OM of float
    | Custom of unit

let sc_curve (judge: int) (delta: float) =
    assert (delta >= 0.0)
    let scale = 6.0 / (10.0 - (judge |> float))
    if delta >= MISSWINDOW
    then 0.0
    else Math.Max(-1.0, (1.0 - Math.Pow(delta * scale, 2.8) * 0.0000056) * 2.0)

let wife_curve (judge: int) (delta: float) =
    assert (delta >= 0.0)
    let scale = 6.0 / (10.0 - (judge |> float))
    if delta >= (MISSWINDOW / scale)
    then -8.0
    else (2.0 - 10.0 * Math.Pow(1.0 - Math.Pow(2.0, -(delta * delta) * scale / 9025.0), 2.0))

let rec window_func (windows: HitWindows) delta =
    assert (delta >= 0.0)
    match windows with
    | [] -> JudgementType.MISS
    | (w, j) :: xs ->
        if (delta < w) then j else window_func xs delta

let points_func (arr: float array) (j: JudgementType) (_: float) = arr.[j |> int]

let dp_windows judge ridiculous =
    let perf_window = 45.0 / 6.0 * (10.0 - (judge |> float))

    let windows: HitWindows =
        [ (perf_window * 0.5, JudgementType.MARVELLOUS)
          (perf_window, JudgementType.PERFECT)
          (perf_window * 2.0, JudgementType.GREAT)
          (min (perf_window * 3.0) 135.0, JudgementType.GOOD)
          (min (perf_window * 4.0) 180.0, JudgementType.BAD) ]
    window_func
        (if ridiculous
         then (perf_window * 0.25, JudgementType.RIDICULOUS) :: windows
         else windows)

let private is_regular_hit judgement =
    match judgement with
    | JudgementType.RIDICULOUS
    | JudgementType.MARVELLOUS
    | JudgementType.PERFECT
    | JudgementType.GREAT
    | JudgementType.GOOD
    | JudgementType.BAD
    | JudgementType.MISS -> true
    | JudgementType.OK
    | JudgementType.FUMBLE
    | _ -> false

let private is_combo_break threshold judgement =
    match judgement with
    | JudgementType.OK -> false
    | j -> j >= threshold

let private dp_based_accuracy name judge ridiculous pfunc =
    AccuracySystem
        (name, dp_windows judge ridiculous, pfunc,
         (fun j -> if is_regular_hit j then 2.0 else 0.0),
         (fun j c -> if is_combo_break JudgementType.GOOD j then (0, true) else (c + 1, false)))

let createAccuracyMetric config =
    match config with
    | SC judge ->
        dp_based_accuracy ("SC (J" + (judge |> string) + ")") judge false
            (points_func [| 2.0; 2.0; 1.8; 0.0; 1.0; 0.2; -1.6; 0.0; 0.0 |])
    | SCPlus judge -> dp_based_accuracy ("SC+ (J" + (judge |> string) + ")") judge false (fun _ -> sc_curve judge)
    | DP judge ->
        dp_based_accuracy ("DP (J" + (judge |> string) + ")") judge false
            (points_func [| 2.0; 2.0; 2.0; 0.0; 1.0; -4.0; -8.0; 0.0; -8.0 |])
    | Wife judge -> dp_based_accuracy ("Wife (J" + (judge |> string) + ")") judge false (fun _ -> wife_curve judge)
    | OM od ->
        AccuracySystem
            (("osu!mania (OD" + (od |> string) + ")"),
             window_func
                 [ (16.5, JudgementType.MARVELLOUS)
                   (64.5 - od * 3.0, JudgementType.PERFECT)
                   (97.5 - od * 3.0, JudgementType.GREAT)
                   (127.5 - od * 3.0, JudgementType.GOOD)
                   (151.5 - od * 3.0, JudgementType.BAD) ],
             (fun j _ ->
                 match j with
                 | JudgementType.RIDICULOUS
                 | JudgementType.MARVELLOUS
                 | JudgementType.PERFECT -> 300.0
                 | JudgementType.GREAT -> 200.0
                 | JudgementType.GOOD -> 100.0
                 | JudgementType.BAD -> 50.0
                 | JudgementType.MISS
                 | _ -> 0.0),
             (fun j -> if is_regular_hit j then 300.0 else 0.0),
             (fun j c -> if is_combo_break JudgementType.FUMBLE j then (0, true) else (c + 1, false)))
    | _ -> failwith "nyi"

(*
    HP systems as score metrics
    HP systems control a meter that fills when you hit notes well and empties when you hit notes badly
    Falling below a certain value will "fail" you - Consequences of this are optional to the player
    HP should act as a guide to push you away from things that are too hard
*)

type HPSystemState = bool * float

let hp_hit_func judge_func points_func failThreshold failAtEnd =
    fun ((_, deltas, hit): ScoreDataRow) k ((failed, hp): HPSystemState) ->
        let j =
            match hit.[k] with
            | HitStatus.NotHit -> JudgementType.MISS
            | HitStatus.Hit -> judge_func (Math.Abs deltas.[k])
            | HitStatus.Special -> JudgementType.OK
            | HitStatus.SpecialMissed -> JudgementType.FUMBLE
            | _ -> failwith "impossible hit status"
        let newhp = Math.Clamp(hp + points_func j (Math.Abs deltas.[k]), 0.0, 1.0)
        ((failed && failAtEnd) || newhp <= failThreshold, newhp)

type HPSystem(name: string, init: float, points_func: JudgementType -> float -> float, scoring: AccuracySystem, failThreshold: float, failAtEnd: bool) =
    inherit ScoreMetric<HPSystemState>(
        name,
        (false, init),
        hp_hit_func scoring.JudgeFunc points_func failThreshold failAtEnd,
        (let handle_hit =
            (hp_hit_func scoring.JudgeFunc points_func failThreshold failAtEnd)
         (fun (offset, deltas, hit) _ (failed, hp) ->
             List.fold
                 (fun s (k: int) ->
                     if hit.[k] <> HitStatus.Nothing then
                         handle_hit (offset, deltas, hit) k s
                     else
                         s)
                 (failed, hp)
                 [ 0 .. (deltas.Length - 1) ])),
        fun (failed, hp) -> hp
    )

    member this.Failed = fst this.State

type HPSystemConfig =
    | VG
    | OM of float
    | Custom of unit

let createHPMetric (config : HPSystemConfig) : HPSystem =
    match config with
    | _ -> failwith "nyi"

(*
    More scoring tools
*)

//todo: add RFC and SDM?
type Lamp =
    | MFC
    | SDP
    | PFC
    | SDG
    | FC
    | SDCB
    | CLEAR
    | FAIL

let lamp ((judgements, _, _, _, _, cbs) : AccuracySystemState) ((failed, _) : HPSystemState) : Lamp =
    if failed then FAIL else
        let c count zero singleDigit (more : Lazy<Lamp>) = 
            if count = 0 then zero
            elif count < 10 then singleDigit
            else more.Force()
        c judgements.[int JudgementType.PERFECT] MFC SDP (lazy (c judgements.[int JudgementType.GREAT] PFC SDG (lazy (c cbs FC SDCB (lazy CLEAR) )) ))