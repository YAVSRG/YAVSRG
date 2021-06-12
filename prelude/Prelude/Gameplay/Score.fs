namespace Prelude.Gameplay

open System
open System.IO
open System.IO.Compression
open Prelude.Common
open Prelude.Charts.Interlude

module Score =

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
        | SpecialNG = 4uy
        | SpecialOK = 5uy

    type ScoreDataRow = Time * Time array * HitStatus array
    type ScoreData = ScoreDataRow array

    let MISSWINDOW = 180.0f<ms>

    let countHits ((_, _, h): ScoreDataRow) =
        Array.fold (fun x hit ->
            if hit <> HitStatus.Nothing then x + 1 else x) 0 h

    let offsetOfRow ((offset, _, _): ScoreDataRow) = offset

    let decompressScoreData (sd: string) (keys: int) (notes: TimeData<NoteRow>) : ScoreData =
        let compressed = Convert.FromBase64String(sd)
        use inputStream = new MemoryStream(compressed)
        use gZipStream = new GZipStream(inputStream, CompressionMode.Decompress)
        use br = new BinaryReader(gZipStream)
        notes.Data
        |> Seq.map (fun (time, nr) ->
            let hit = [|for i in 1..keys -> br.ReadByte() |> LanguagePrimitives.EnumOfValue|]
            (time, [|for i in 1..keys -> br.ReadSingle() * 1.0f<ms>|], hit))
        |> Array.ofSeq       

    let compressScoreData (sd: ScoreData) : string = 
        use outputStream = new MemoryStream()
        use gZipStream = new GZipStream(outputStream, CompressionLevel.Optimal)
        use bw = new BinaryWriter(gZipStream)
        Array.iter (fun (_, delta, hit) -> Array.iter (byte >> bw.Write) hit; Array.iter (float32 >> bw.Write) delta) sd
        bw.Flush()
        Convert.ToBase64String (outputStream.ToArray())

    let notesToScoreData (keys: int) (notes: TimeData<NoteRow>) : ScoreData =
        notes.Data
        |> Seq.map (fun (time, nr) ->
            let bits = (NoteRow.noteData NoteType.HOLDHEAD nr) ||| (NoteRow.noteData NoteType.NORMAL nr) ||| (NoteRow.noteData NoteType.HOLDTAIL nr)
            let bits2 = (NoteRow.noteData NoteType.HOLDBODY nr) ||| (NoteRow.noteData NoteType.MINE nr)
            (time,
                Array.init keys (fun i -> if Bitmap.hasBit i bits then MISSWINDOW else 0.0f<ms>),
                Array.init keys (fun i -> if Bitmap.hasBit i bits then HitStatus.NotHit elif Bitmap.hasBit i bits2 then HitStatus.Special else HitStatus.Nothing)) : ScoreDataRow
            )
        |> Array.ofSeq

    (*
        Score metrics - These are processors that run on score data and keep a running state as they go
        They can then output a value which means something depending on the metric
        For example, Wife is a score metric that gives your running % accuracy according to the rules of the Wife scoring system from Etterna

        Used for: Accuracy, Health Bar, Performance Rating
    *)

    type ScoreMetric<'state>(name: string, i: 'state, row_functor: ScoreDataRow -> int -> 'state -> bool -> 'state, hit_functor: ScoreDataRow -> int -> 'state -> 'state, value_functor: 'state -> float) =
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

        member this.Update (now: Time) (hitData: ScoreData) (playing: bool) =
            while (counter < hitData.Length) && (offsetOfRow hitData.[counter] < now) do
                state <- row_functor hitData.[counter] counter state playing
                this.UpdateTimeSeries hitData.Length
                counter <- counter + 1

        member this.ProcessAll(hitData: ScoreData) = this.Update (infinityf * 1.0f<ms>) hitData false

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
        | NG = 7
        | MISS = 8

    //judgements, points, maxpoints, combo, maxcombo, cbs
    type AccuracySystemState = int array * float * float * int * int * int

    let accuracy_hit_func judge_func points_func max_point_func combo_func =
        fun ((_, deltas, hit): ScoreDataRow) k ((judgementCounts, points, maxPoints, combo, maxCombo, cbs): AccuracySystemState) ->
            let j =
                match hit.[k] with
                | HitStatus.NotHit -> JudgementType.MISS
                | HitStatus.Hit -> judge_func (Time.Abs deltas.[k])
                | HitStatus.Special
                | HitStatus.SpecialOK -> JudgementType.OK
                | HitStatus.SpecialNG -> JudgementType.NG
                | _ -> failwith "impossible hit status"
            judgementCounts.[int j] <- (judgementCounts.[int j] + 1)
            let (newcombo, cb) = combo_func j combo
            (judgementCounts, points + points_func j (Time.Abs deltas.[k]), maxPoints + max_point_func j, newcombo,
                max newcombo maxCombo,
                cbs + if cb then 1 else 0)

    type AccuracySystem(name: string, judge_func: Time -> JudgementType, points_func: JudgementType -> Time -> float, max_point_func: JudgementType -> float, combo_func: JudgementType -> int -> (int * bool)) =
        inherit ScoreMetric<AccuracySystemState>(
            name,
            ([|0; 0; 0; 0; 0; 0; 0; 0; 0|], 0.0, 0.0, 0, 0, 0),
            (let handle_hit =
                (accuracy_hit_func judge_func points_func max_point_func combo_func)
            (fun (offset, deltas, hit) _ (judgementCounts, points, maxPoints, combo, maxCombo, cbs) playing ->
                List.fold
                    (fun s (k: int) ->
                        if hit.[k] <> HitStatus.Nothing && (not playing || hit.[k] <> HitStatus.Hit) then
                            handle_hit (offset, deltas, hit) k s
                        else s)
                    (judgementCounts, points, maxPoints, combo, maxCombo, cbs)
                    [ 0 .. (deltas.Length - 1) ])),
            (accuracy_hit_func judge_func points_func max_point_func combo_func),
            (fun (judgements, points, maxPoints, combo, maxCombo, cbs) -> points / maxPoints))

        member this.JudgeFunc = judge_func
        member this.Format() =
            sprintf "%.2f%%" (if System.Double.IsNaN this.Value then 100.0 else 100.0 * this.Value)
            + (let (_, _, _, _, _, cbs) = this.State in if cbs > 0 then "  -" + cbs.ToString() else "")

    type HitWindows = (Time * JudgementType) list

    //to be used in score metrics/displayed on score graph ingame
    type AccuracyDisplayType =
        | Percentage
        | ProjectedScore
        | PointsScored

    type AccuracySystemConfig =
        | SC of judge: int * ridiculous: bool
        | SCPlus of judge: int * ridiculous: bool
        | Wife of judge: int * ridiculous: bool
        | DP of judge: int * ridiculous: bool
        | OM of od: float32
        | Custom of unit
        override this.ToString() =
            match this with
            | SC (judge, rd) -> "SC (J" + (judge |> string) + ")"
            | SCPlus (judge, rd) -> "SC+ (J" + (judge |> string) + ")"
            | DP (judge, rd) ->"DP (J" + (judge |> string) + ")"
            | Wife (judge, rd) -> "Wife2 (J" + (judge |> string) + ")"
            | OM od -> "osu!mania (OD" + (od |> string) + ")"
            | _ -> "unknown"
    
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
        | JudgementType.NG
        | _ -> false

    let sc_curve (judge: int) (judgement: JudgementType) (delta: Time) =
        if is_regular_hit judgement then
            assert (delta >= 0.0f<ms>)
            if delta >= MISSWINDOW then -0.5
            else
                let delta = float delta
                let scale = 6.0 / (10.0 - (judge |> float))
                Math.Max(-1.0, (1.0 - Math.Pow(delta * scale, 2.8) * 0.0000056) * 2.0)
        else 0.0

    let wife_curve (judge: int) (judgement: JudgementType) (delta: Time) = //TODO: update to wifev3
        if is_regular_hit judgement then
            assert (delta >= 0.0f<ms>)
            let scale = 6.0f / (10.0f - (judge |> float32))
            if delta >= (MISSWINDOW / scale) then -8.0
            else
                let delta = float delta
                (2.0 - 10.0 * Math.Pow(1.0 - Math.Pow(2.0, -(delta * delta) * (float scale) / 9025.0), 2.0))
        else 0.0

    let rec window_func (windows: HitWindows) delta =
        assert (delta >= 0.0f<ms>)
        match windows with
        | [] -> JudgementType.MISS
        | (w, j) :: xs ->
            if (delta < w) then j else window_func xs delta

    let points_func (arr: float array) (j: JudgementType) (_: Time) = arr.[j |> int]

    let dp_windows judge ridiculous =
        let perf_window = 45.0f<ms> / 6.0f * (10.0f - (judge |> float32))

        let windows: HitWindows =
            [ (perf_window * 0.5f, JudgementType.MARVELLOUS)
              (perf_window, JudgementType.PERFECT)
              (perf_window * 2.0f, JudgementType.GREAT)
              (min (perf_window * 3.0f) 135.0f<ms>, JudgementType.GOOD)
              (min (perf_window * 4.0f) 180.0f<ms>, JudgementType.BAD) ]
        window_func
            (if ridiculous
             then (perf_window * 0.25f, JudgementType.RIDICULOUS) :: windows
             else windows)

    let private get_combo_break threshold judgement combo =
        match judgement with
        | JudgementType.OK -> (combo, false)
        | j -> if j >= threshold then (0, true) else (combo + 1, false)

    let private dp_based_accuracy name judge ridiculous pfunc =
        AccuracySystem
            (name, dp_windows judge ridiculous, pfunc,
             (fun j -> if is_regular_hit j then 2.0 else 0.0),
             (fun j c -> get_combo_break JudgementType.GOOD j c))

    let createAccuracyMetric config =
        let name = config.ToString()
        match config with
        | SC (judge, rd) ->
            dp_based_accuracy name judge rd
                (points_func [| 2.0; 2.0; 1.8; 0.0; 1.0; 0.2; -1.6; 0.0; 0.0 |])
        | SCPlus (judge, rd) -> dp_based_accuracy name judge rd (sc_curve judge)
        | DP (judge, rd) ->
            dp_based_accuracy name judge rd
                (points_func [| 2.0; 2.0; 2.0; 0.0; 1.0; -4.0; -8.0; 0.0; -8.0 |])
        | Wife (judge, rd) -> dp_based_accuracy name judge rd (wife_curve judge)
        | OM od ->
            AccuracySystem
                (name,
                    window_func [
                        (16.5f<ms>, JudgementType.MARVELLOUS)
                        (64.5f<ms> - od * 3.0f<ms>, JudgementType.PERFECT)
                        (97.5f<ms> - od * 3.0f<ms>, JudgementType.GREAT)
                        (127.5f<ms> - od * 3.0f<ms>, JudgementType.GOOD)
                        (151.5f<ms> - od * 3.0f<ms>, JudgementType.BAD) ],
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
                    (fun j c ->  get_combo_break JudgementType.GOOD j c))
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
                | HitStatus.Hit -> judge_func (deltas.[k] |> Time.Abs)
                | HitStatus.Special
                | HitStatus.SpecialOK -> JudgementType.OK
                | HitStatus.SpecialNG -> JudgementType.NG
                | _ -> failwith "impossible hit status"
            let newhp = Math.Clamp(hp + points_func j (deltas.[k] |> Time.Abs), 0.0, 1.0)
            ((failed && failAtEnd) || newhp <= failThreshold, newhp)

    type HPSystem(name: string, init: float, points_func: JudgementType -> Time -> float, scoring: AccuracySystem, failThreshold: float, failAtEnd: bool) =
        inherit ScoreMetric<HPSystemState>(
            name,
            (false, init),
            (let handle_hit =
                (hp_hit_func scoring.JudgeFunc points_func failThreshold failAtEnd)
            (fun (offset, deltas, hit) _ (failed, hp) playing ->
                List.fold
                    (fun s (k: int) ->
                        if hit.[k] <> HitStatus.Nothing && (not playing || hit.[k] <> HitStatus.Hit) then
                            handle_hit (offset, deltas, hit) k s
                        else
                            s)
                    (failed, hp)
                    [ 0 .. (deltas.Length - 1) ])),
            (hp_hit_func scoring.JudgeFunc points_func failThreshold failAtEnd),
            fun (failed, hp) -> hp
        )

        member this.Failed = fst this.State

    type HPSystemConfig =
        | VG
        | OMHP of float
        | Custom of unit

    let createHPMetric (config: HPSystemConfig) scoring : HPSystem =
        match config with
        | VG ->
            HPSystem(
                "VG", 0.5,
                (fun j _ -> 
                    match j with
                    | JudgementType.RIDICULOUS | JudgementType.MARVELLOUS -> 0.005
                    | JudgementType.PERFECT -> 0.0025
                    | JudgementType.GREAT -> 0.0
                    | JudgementType.GOOD -> -0.05
                    | JudgementType.BAD -> -0.2
                    | JudgementType.MISS -> -0.1
                    | JudgementType.OK -> 0.0
                    | JudgementType.NG -> -0.1
                    | _ -> failwith "impossible judgement type"
                ), scoring, 0.0, false
            )
        | _ -> failwith "nyi"

    (*
        More scoring tools
    *)

    type Lamp =
        | MFC = 9
        | WF = 8
        | SDP = 7
        | PFC = 6
        | BF = 5
        | SDG = 4
        | FC = 3
        | MF = 2
        | SDCB = 1
        | NONE = 0

    let lamp ((judgements, _, _, _, _, cbs): AccuracySystemState) : Lamp =
        let c count (zero : Lazy<Lamp>) one singleDigit more = 
            if count = 0 then zero.Force()
            elif count = 1 then one
            elif count < 10 then singleDigit
            else more
        c cbs (lazy (c judgements.[int JudgementType.GREAT] (lazy (c judgements.[int JudgementType.PERFECT] (lazy Lamp.MFC) Lamp.WF Lamp.SDP Lamp.PFC)) Lamp.BF Lamp.SDG Lamp.FC)) Lamp.MF Lamp.SDCB Lamp.NONE

    let grade percent (thresholds: float array) = 
        let mutable i = 0
        while i < thresholds.Length && thresholds.[i] > percent do
            i <- i + 1
        i

    type PersonalBests<'T> = ('T * float32) * ('T * float32)
    module PersonalBests = let map f ((a, f1), (b, f2)) = ((f a, f1), (f b, f2))

    type PersonalBestType =
        | FasterBetter = 3
        | Faster = 2
        | Better = 1
        | None = 0

    let updatePB (((bestA, rateA), (bestR, rateR)): PersonalBests<'T>) (value: 'T, rate: float32) =
        let r, rv =
            if rate > rateR then (value, rate), PersonalBestType.Faster
            elif rate = rateR then
                if value > bestR then (value, rate), PersonalBestType.Faster else (bestR, rate), PersonalBestType.None
            else (bestR, rateR), PersonalBestType.None
        let a, av =
            if value > bestA then (value, rate), PersonalBestType.Better
            elif value = bestA then
                if rate > rateA then (value, rate), PersonalBestType.Better else (bestA, rateA), PersonalBestType.None
            else (bestA, rateA), PersonalBestType.None
        (a, r), (av ||| rv) : PersonalBests<'T> * PersonalBestType