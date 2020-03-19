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
    | Not_Hit = 1uy
    | Hit = 2uy
    | Special = 3uy
    | Special_Missed = 4uy

type ScoreDataRow = float * float array * HitStatus array

type ScoreData = ScoreDataRow array

let countHits ((_, _, h): ScoreDataRow) =
    Array.fold (fun x hit ->
        if hit <> HitStatus.Nothing then x + 1 else x) 0 h

let offsetOf ((offset, _, _): ScoreDataRow) = offset

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

let compressScoreData (sd: ScoreData): string = ""

(*
    Score metrics - These are processors that run on score data and keep a running state as they go
    They can then output a value which means something depending on the metric
    For example, Wife is a score metric that gives your running % accuracy according to the rules of the Wife scoring system from Etterna

    Used for: Accuracy, Health Bar, Performance Rating
*)

type ScoreMetric<'state>(name: string, i: 'state, f: ScoreDataRow -> int -> 'state -> 'state, g: ScoreDataRow -> int -> 'state -> 'state, h: 'state -> float) =
    let mutable counter = 0
    let mutable state: 'state = i

    let row_functor = f
    let hit_functor = g
    let value_functor = h

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
        while (counter < hitData.Length) && (offsetOf hitData.[counter] < now) do
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
            | HitStatus.Not_Hit -> JudgementType.MISS
            | HitStatus.Hit -> judge_func (Math.Abs deltas.[k])
            | HitStatus.Special -> JudgementType.OK
            | HitStatus.Special_Missed -> JudgementType.FUMBLE
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
              let mutable p = points
              let mutable mp = maxPoints
              List.fold
                  (fun s (k: int) ->
                      if hit.[k] <> HitStatus.Nothing then
                          handle_hit (offset, deltas, hit) k s
                      else
                          s)
                  (judgementCounts, points, maxPoints, combo, maxCombo, cbs)
                  [ 0 .. (deltas.Length - 1) ])),
         (accuracy_hit_func judge_func points_func max_point_func combo_func),
         (fun (_, points, maxPoints, combo, maxCombo, cbs) -> points / maxPoints))

//todo: ability to enable ridiculous timing windows
type AccuracyDisplayType =
    | Percentage
    | BestProjectedScore
    | PointsScored

type AccuracySystemConfig =
    | SC of int
    | SCPlus of int
    | Wife of int
    | DP of int
    | OM of float
    | Custom of unit

type HitWindows = (float * JudgementType) list

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

//score data wrapper with lazy evaluation goes here
//no it goes in ScoreManagement

let score_test =
    let data =
        decompressScoreData
            "H4sIAAAAAAAEAO19f3gVxdXwchVBo5QAuooCIRSNoKtoEAw/svfCXSmpKBaLomsxisZfKHYpNooNkSi6goLUWDSW3hWJohFpRF1+ZPfWKIJSV9FYNBbTUvdKTUHTYlHsd3fm7syczcz1/d7ne77n/ePdx3lyOZ6ZOXPm/JqzM7MxKRbTRj2vStnH7xznhL//Zz8x8st64+44C9LSVpyHl1y7vwXgPXIWF8/6ZgFsb8wgPt49/R2A11Dr/reHob7E7UMvqQe0ZC45hosnbRvhsiD9wfcENJ8I8KTVF/LxNt0c4iFgZmsh+rdn1MZhx5JkXPCPOKB5ZEmCbc5YVM/whalaPA3g6SUyyz/Qh1zXDvqwJ94H+GKP/YUqqms/uMAFdasujeeQYslXF5A+/eKR8VgWaK/eGLelKi5fTOXPSIYCPKXsUzUoon6Vvn90EfueWoL6UE7plWaxlOrrWb6Quv5PJ7jKMT9zAoD+bu8022Y4H9L7KoIbv/QctlvDXeCEXXjD/uVG6qJHO+0hlwVZR2+JyC2lxdCnukEJ6/q177aIxqs/dQ2gxWs1y0Mkw7te5Y3DnHYEotc2PgRzlHmsxiX0XTQhLkUqE7yOgpZuwKDdq5rRXHrJLyYGRUSz8cU58YAU+W0F0RH+DbG8W44UyqRyaLAKdOZcXyXjPTA2ynuEqMx4C8GN5D1AnqW+A3HdQP66Lo6b036n8rr1n92nsu1ZN31YHv5FgCtvEPJKb1obB3X7b4mHfwNAZvtJ0bqksjf8x25QiAzV6wQXgWr+0iKqa99xlxsUIvfb1wl5ahy9AtFivPooV3Ztfz8zl4ycfrrGCQqh7/pXVLaq9dGPozJOfil3Lyay5ncoqiRCzD7e0lIyDi+5mGsjtIMj1BCi72qC7YV1h/ZMsKDM2L4JCSLSsb14jhOUsK48f0Gch5fRat2gBAC5fRicy5AvBb+Ld4fmaGo9vpzY4ocP8v3CyGcd9KNHCCmdJGrPrHwC2T/761dQHbvx98D3eG9fxh2HN/wtNyhkvId7q7SDLF7NNbi9qt+oQSGyce3RDA+Z+ej1rhP2IA96oTwo3LGZAx0WpPSLATupTD1eqB/ymw+53uGTCM3W4zcw+hGLmX2uQ21755amzY390wFMG9KOYFbFBjUo2dZiljm7PChoPIHOsL6n800312tM+fLDeFBQ3+2vO8C/Lfksx+ccfY19XPnAA6ggXahY6IBRZBGtay5AdYK/9h1rqb1Pt1O9ybWn3TrZtUtud4KCZO35FW60Pf/1TQhmn7jbCUrYnrnzSeh7Rp8A/RtrN/YMdoNC6l71gsPW9b6I+IUcfebc7xwv+dzEoCD6Br0g1v0NOI4IeKpfe48blODfXvLyiSye3DDVYata4z5QAf9GPo7+HfxVDv2MjNdPWWL/YS9z0FxOvcINCsIvHg39wis7w1hFMgotla1P4quTahmeZmkpWIZscTYWiAcF+ZTe/3CCwuVB2dYcHxmbM2+tExTCe2UwX2cKdrWEELvkKq7N8W+fzM55TJPfIHQYQ6uAjGunkfgW8Mqv3O4G1Fk3tatB4dGSWfHb0I7H5C3nE1q0tpQDmit6B9ghY97l3PhU/mIB4b21Y6YjgYe1zxscs+ksYoekmnEOD09Zsd2V9t+HdNCvdGB7OZ/sd/wUwa2KJwFfkjN/FcppVka+KQ8KqM081shXgQwpqyYQvphNJ5CxWv+5NkF98jxuLJpckInbRSckkL6N/jlf35SV5dY9y+JBQbwyn3F5eFnf3WJVrHSDgtobfncUD/2Rn/+JGxTe2JSy67A8X7bIDQqPBfbY0WTesjG9A/vI4VTVATtkDv79//3aTfgwMde3cxIAVPMl1696/zwi4v85zRW5DreToJ9d90Z8Ewft4VOh3Pe4gEuLMmIeWFt5c14FsYrXsjUaq5Bf/uArwXj1H68Rj+t/n/+PD9Vpc+4Hjt51sBzY3bZlTohlLjvNDQqvrrT6D47efByNuRrHcnUwc/sKGOu9tA34AHnNd+Tf+Dfj9/en3KCQusN7CH2otOcbB/mFc303KDw0+YaCNK+ucXkL7aP1RpVXN3lRJ7Wdy7L+MPco1ZMJDzSzJM2rqw84oIYQ4/ixMM4J7e7AjVw+a0NWuNn1JbKTWT8ctU0wbq8bD3yFta8C8r5nh8NUjPmH3qY5D/SbkYPfruTaXd1YTtYGmcqjVR4ptr+6PByFVv+aExQRzZI0icQWxvQC4ZpEkuarwDZt7wfzXKVb2LFJ1p1/E8Z62gt90izIW7iTLy/z7+fyQJ6/OJdTODJtuK38uvo8NYQY0xdF1sTR9dYQGjOo/RwRnnH1YORXzYnF0ViZ/FLsqSC3aTzTzqXPWqfSeDKXQxL261US+uRFvdIsknfm34V89t+dD3OMY/ty9cO68xKiW96gFI1L3nsuTmQttZI7juRn14M+rB29HPE4bCDjysAGmlsquDyvbin29okkjo2dKbZDzpkqkDV5y2YWS25IEvrQXN79MyH/9K6cbHHX2Aze4qMgn2ueE8aE+g8/RjKk2F9CmQzt+BPlYH6tO5uFeSRj0SkgPtAuGs/4+B5Mv6WTvJeqsQ3z7nRge5GY9Yo+YI7kOX8m8a7e9YgbFFFdsylGc0tlRQ4PzS/e2UIhl0RzVeSXUlvHXX+Yd1eQWFnqeyuIHbPzFRkbw6seJuCVdPGvhLbO7PxGRXM0MAVlI+Tzy0O4+RK5YbEbFATY9qpQrqS+E4jua/LYvHJveKfA3INSMzGHFNMeeoXU1TevJLpqzh3i8JqTD+M8B5KrnyzP22+meKkD5i3dQnTVS66dKMEH6qD6I7Du0S56Hcaxex/h0me9MCzBgvzKqSqDFTO/m0/5fllxAsjLuz9PUFTmuWoFkZesv1Dp/2DGVjsxEUL0rj8xtFE0I36lOF+XPJr4ZGtdEuRjLXO2UMbtqmEqCzI7W1QGK2YVbKK2+KYVgKdm06egrnJI468hPv8HkbVM6gesXeo2Dml/M5Eh4+o6bnta/XqoR86ZYj1qOgvQnOVTmDfLytcKNyiiulbFLGQnvZeahXpkbVoKaDHmjYP2wIipkbqksrLqp/GgELvri+2B4eFceCZV54hosf131RCiyeeW89CUVZ8L+/CLA91kdPXMh4m+yfoTeXVVL/2cxHB+apYjxOvaDvvQ+5WzWPa2P8Kc4DePiWO4mcPJXOp//dr1khuFc5mVu5zv+YmwPaXskMOCvNanHBZLN2qF7zUM7z0nKKTu6NfFcWzN0U4AsP01YvustjghRCk7Nrqeh+2d3QBjGvsMlYcnL1yEY0dlpVie990A4gileiQ35jK86x1eH94XfYmtM/8lgTgik3qA68u8Nf/i+jJp/RY3KBgw3JHgA2X3nFcBD+wlp/LXLisH4HdKV/+Z71flCeluwKDeX9+PxFdiWfOLrwc2R59RzI0jMhsGJdC74xEtblC4/Q5oIbZTGTiOS7NuPKqKaDGvWgPXgyffw+WL3fgMtUNTZnJpkWquUUOI/VgzlxY/NV5lQfrKzVFZIw1m+eSYnStU2sUahz+MvwEboR08AdgI6eI8elT0FXe8furrFuqTW7n5bN3AeV5k/wb1EsbAtj8x9AEIaDbhGMHcmkC5UrPzHlUSVZYCn3Ei4b03pxcc255p4rWB8TaQSa0N5NxBH9rME0gfyqFOIZ61aQmQXfvERuDfrCvoe84gPvB6rnDCf2eqB4B4SJ+c4cq99vNliC/GIls4NvvrPaBfU2ksZ7GM6bdG6kb03LgOjsO/UOXhmY8eReyVvOVj/jq+4UWHBVnj7gVzpL0xgLEZsK5WfwS3X/mWWpw/eOnVtOENTfDqeq3Tcd3vWfvJ+p38sb3em4zN+uosYIu1z9YLeZ/p6EHWW1bBF25QuIhB3xNuoO9m9X5c/pk7S0gcoZTN5NoNs/NmYR/KrJ6kD934ZwsfbbkaQuyqlx3R2OzecA3m7RXrjJ/aDfKiUg3e4xXwxRi1zg2KqK5W/x6xa4b3O/77skl43jGkSByXTJ2d6AbMPvLxY+g6tGgAI0MR+/Lpp0AXFPtbshfHjw3j+5TmaXli0QNbA4C1L55XB73kgvLQ3GvyDx3psR+kQyRZP8URVTW8Du4+nuTM26gNq+5QuXULv0ZwpFt1X6uavGiriD7duNIBetSee6dC5m0oV9ZkfRrxydaO277HDr1KbXvrKkeEZz65M2Kv9pEchf7avrx92Hcsi4d8NpuWu/Ydu2hOa+QfxHJ1qEYNIabyuspDM+LbwFpSqQZ7RrrR4lc+R2LgwMaJ8JSy+Q60sb7DwzMWvYTjtQOzuXIqLz0m3Q0YtD8rA3yjPWWrUFcN71Tcx9FnR+x41B64wB4oZVdC31gwSuVV1WZOSQBaGmcDnnq/bsjLU2WVgXxFwIug8NBs/20QIymrusQ+3lwFxmFVWCoPTzv4Nl4vzJ0gjkFWPgR4oBuFDg9PHvQxNxawNrUQ/dDqaawThFKZDUei+fAufZOfhztqIYojFPsTRwIPa+8XwVxBzSCHxTKVASqsG5XT5USetQU1/PxG9e8cFqQ/1yPBYunGWZE+GB9fR9YVCOhdivf4GvM24r0Bsd357cvkwyC+90Y/BOTeuulpEKvIW+YL59Krq4FrnD06d7zG9DccOjaw57U7fcajDrJD5t0TRWh+7GIY600byu3X60nXQtlYN7/vqXtfhfrxKFeGjAOv0HVP1aXc+MU7/j6av7o9mgdhdMEYwZfxj05PoxxFc4sbFF7dQMcJxCnJP+ddt6gYsCaa40FgfcCpqC2l+k0HzpvhsK3J+nrh/l2z8zM8b5vWCcdrLIf7zL3kQUfUnt34Y+DfMrW96HjPFud4rH2/obHouqPdoIj6kKQHt4L5uPYe7nx4779O3ykl7uXOh7VvGI1ZP/8H1/cYo8R71O3Ljk/TWArnvLv1UUD8dEypXkpw0G8Sg3yjsjWN5Y8zbUW6LdoNczf2Lm7caZeMEqyJz1SxjC/JK3/G9FUkttXkj/hr55US4Z+fZu2IRORUOQbbGy85CvrVVUOgT2l+z2F7sHtH9+cwMnkgsq90ziXs/FJEdb0jvfOii23Y8s35xis5h8l47SriK7vhGYXPq8HQzJ25/bYXt4Fx2I2L1GjNbIzM3cPsrXkpK+tVAX0xNl8n6wdawHifOS4d1lQeqHWDwiMPxWUsLf4LwnEk6z9zgK6mHiCxqJe8X/guISnvoTpdNCA655j+w/1Y204qG16ZGxTkoybckN/+7TLpmnNCnK+/BSmiW7Y/k4zV2/sc8JeK7avRukr1PGJz9OaX3KAwXWDZtb/FsP2RvKO/nJwJ8Vck0tbJQ9OicZjLzqPvKu0F3HF4PYldiOigQmyEqdSROUH64f/TkcDDzFF97uxFqFsDjgPxgbnz5by8l1b/jfJ+Dj/21nf9kmtf9K7H8BmY23Be2Oz8BdTV1h9TukPY3hlceQliw6Cg+VXpnht98g4wH4anh3shEFBeeBTuW3kSrWG1MYu5tt3be54wfpYHLYcyVN0b6rlP8/VIZ/bekpenfqqRzmUTfocTIpn/GsPKj8TywBoZS+sziqmf6VqrMniUXn09We9Lzpnc8dpVw1iaY5Z5Om1r/8Q0INkZD+ZNq39JyCt9xi3Yxg54OsID1t73JTZM1rMxEpWfbu35Kz5AMiQvNBCO2fQfGO+az6i8qlp9P2KblOp6Boep+/hD3LhT1j/k5iOsO1fj94N1Ut751eSHSa5Z1mWVRbIbFXHsM/J8MLak/EuViyih+FbFYwtiPn572mlk/QLjoeS3hD77a2bvKJHnfYxsxMi5N2PU0BztzDvNattRqivRnCfrK1l6iQ5KNdfg9tZ/hO394SPyrn+9+c9EbPZBlcUym87nzpu+WUqwIPmWx4Feagd7CXmvz1gG12Vz8LtZu+b0wOZI1smjI3oZkY1rhhC9zBQv5dp24/zB6Rwk5qfJ+6rsfDgk3lDK6LsOGjNsYmMu0qBdtcH1vhiIbGLm3UJCH/pNeHWJyqtrTH/LVSp7obFpCzQhX+SFfWHOg9E3bF/+7MC6UTkd4QA+d+1yeHh+Kk5kUrG3A/8mL3xFPG8r4fpXku5zuIio3V0O0plVRrQ9gufV1blBIbZugdZNP5TqDqAf1r470L/Dv8F8GMtvIzhe8o8ghrOrXBJveHOK4kHhkey1/l5lQWYn2CcO5W/How4Aqf1IPG5Xzcq/dk5uJLzPpGpUFkmxNzqRqtQnFz/hBIXajSLu2kBeOIfYYsObrErsE8p4z95cHmjyvaEvi/kx6rMCu0Pm41lJyBe99Ch0xtP2n0X92petAvISvB/mVVXsa9QQYplNkfUCM0eXkr0pON5ox/zTrzwe5ar8sui+zWhcN5vovqzfzM0/680/onkQeVFe2+mnbnBYkDn4t1wfoL38AfJlfuxi/lp3aAPXhsl6bi9fqB9t7L6qSNy0UAE5AGPediL3yqHdblBEdb26wfEoLFMsk7y3tmC8a5lXOGF7uvGrrB+aLGxPNwpJXbk9Hg9KiGTunBXhKTO/DQXAp5hNh4Sxj7VjOBzvqOuB7ZQP88/lh7kgAtoDztdmY7MfknEph3aobF0/tdvh0SJ/QffRSjVHc/v1U7n3dLEgXjpVDYpobMostCYFMLNzpBvy1FtzStyYdzvwofoufGYS/2XinB1nMfZvohsUUb/Snptp7qaf4Mxt2yAyl9Z7R7K2oHt7q6sBLX7xzYD32kHxnjtz2lC0f8N48WdxvWujykNT7E/KeXXtbRvJOOQt14E1mF2yIKKDsRxtv4hnUuS8PpTnAe3d9EObOQLZOq3+Q1QnSyPhs9zwiWtOY3KZYZxTtwHB5DMnoFhAN87P6yusdRvp+dCUy+WBXjoWyr3eAd/lLz9LHAMXjILzMfNk7nowK7vxoCD9Vc7Ju9deqw/if8aGNRSDPszOpex8xMzvYsRmW+ZA+E7YeR34Dz91rVBe7K97IB8gt58kHK9f/HcyR376pXhQRO0pU89N4DOAN8eJ3EbQzJ0rwdj05lOEvlE5dAyiD727Zp8e4Q+w/4DyPj0yYXgdxP/qr13I+DiGz+0Zopfe8O/yypXhtcM817wGKAf1MG/h1UXyTazNTp7m5EAI6Kd2TKR/GV71+TQerWtXvc7ozEFXcq6FsjHXJP82FQ3QrDefjf4f3gVfOkmxWR8akY2qnnTfq7eOv57pOpH6/foljmi88mE7Is93Al5pY/pHYhCGlD3fAJtt+/i8vLcG7z/1kg159+/a/n4iB7K+RCy7dtUWbJueUIN/kz2jOSyl+uxucbY0ewgju+w5tDVukJcK5kOp7k19Y/A7bK/sBa7d0Nb2SnvJqWkMWO3kG5vc/g49f94UWUvWrxDqtCQVqizILqkS8kWTp5P4L5KTCc6h56VPkx9Wkc3uSgp1QbEvIusAq2KwcN2jvXyvGxSi012bus1H1o8nKCi77jF/j+jzkp+QdY9XR++20NqOBOt4b/gb1O8n+7cEhUezZf4arjl3fCrkn9k0AvSh73qKxnXKMDUyXvLLmO5x42LJeR/JvdaWFPLKL74zbjbtI/NmFWxi+mFsSUmK0GJXfe3wSJF1O5KjeFbYr1VQ5QaFxEOpH9A2Q6zVJ7M6Q37pR+J1CLpzoOztbFzxAOrXKKwB/MN7HmhVa91fhLyXpClI/ozCCu4awvykP19/nUtdre05fA68VXy/jzxoDVhbSc7P4uFfrB9HC+2a3diXykGnLOwjK/NcOTDc0xJYTu8vl2r+4vDqSuvx2gDpYGl0XUbHqxvPqqZyKY3/2l0hT7N+sByPba2Q5qQ8x2FBZtN+sF5Q7BlkPvzUpSqQA6MQnBX0E09xfY9UVAd00E81gj5sP7d3PnhmL4J6njzPoaj0sf3hcdre84J9fb3UEGIVfMKNixX7yHJp9lvcedOvLaN75KQTxLKr6mAdoM2MkbWQt6beDYqoLnrPGgvHOsrlodlVf3XIOCpmieW09ytQxtf3SON278L7QTqmR+pG4nHzLWKHsraEy1NN7kHeB5h9JsL2Qj03n8njuw862IbdkTeGk/UP4Xtdu4/KYskNDpdXUs0ZRI+yMVXeeMNLPtKC+JKy4jws9jwx4v2OFyJ6BNvLxtHQf5ROAPF9Nv7nr507boxTyJS8fWA+UJCpNJJ3hpL0cTkunKrOMREdHOaAse37KjIfbB+DXQgKzlIL6Ks+xcU+75O88yvteRr6ikoH9GE2HQtjZWU8jc3KVtPYYtdTYI4Mr5ylDXTrp+D5FL10Tp5YasLWQI0UO7Q/uXfeoWx434jXaiX9CX3G8kPcOfeSRzqU5g+5Pk8pmynsQzdeBesP25/Et2ErXeTfkvXf8P2qUqEyQBof7GuMB/tukD8yalUJPOw4VGBz5PYx6N/W45fiffreOkcSVQ7+/6ib6d0x9gxWX3tI4AnWj6y8fAHmMtjzwHagHOomz7TybSVpVEKI8zLXliRlvOcJ+dCe7+WXZ7Uld0asUuVheXuXcfvwkosdFmSZexwGK+bNofdd2H7tFmCLxzQAHmjyEKZvdhxdoA+54RqgM6byc0cCD2tPL4/ofpFY7qvGAz9oKse4FOnRSB9R35MgftAvfo07DpIPC3la9zOxT675kwrjZxLHQDxpG107F47nzhGbE9Tkj/LKgbnTgj5gwHtknWI2rXaDIqrr7f0LPQOt9FS5aDWfqCxIq79HZbBicsMywhM/4YH5yMZJwAcYhSUgDsskaD5CqvlSBePY9QzXvph9/hbH/jxyboLENH8C+bqsDIvPQY5a7xovfsXci/hHdn8T+ZUc44NxwPwNxbM2lSVYkFL2KJB7ZfvIBKQl4s9TMD+uHXyc5nMmPO8GRVRXa1uMbWfXuLz+XDu4DZ/NjT0pjPm163skzJ3YjntrPhTG8sa8ceF4EdDatxnjzv4O71Ef1ZGXFqmmlTlvsIjLUz/mwvV0RUaog4bbB/uAwjF5dcb2r6N7CLb9Kc8acZIaAORBl4jXM/W/EPjBDRHbNAvxCp3lTklOUIT0TTkfjSNrU1gcgGburCE2wvD6qZIIUcLnOpD8VfcWjoOskUK+TClKcBGDvjtHJ1iQfGZBgofnHT+JWc9UA3/pJRvgHIXro0MVcWn2U1zbqXcNYs4FbubroPnDRAix/eDMDPuwLj6XHyaxwBcqSw1by/ZL4xDUi+il3tWqBkVUN3N7PzSXemmBkPd6cyXNtfR+mLs2kGo2CeXU73gGz29ZUV5985JXkfeIhvdaFvdY4i/tqueF9Fkj3wayAfdQMXKwpZDwXm7/KjoObCMeH5ygfxn5qzwD2E4v+ZxwvFZFT/x+urIqr57jM6aMPa1fwtVVq+Av6MxKkBPHefHuzfmpn8Oz/36zw8PD7wIwJBtbce810NoaxHOZ+jNaI0qro/ehRMfWQHWh7FkuLcrubXDe5HMdgFXTbX8E+eXNuS+Mw3JA7Mv0riq8X+q+29P56JP22DT/rGxQuX307A1j6tE/SPDwdON8wlOl7Hy+fkgjhPZKcp5E+/C0g/hcle0/yn3PqRuFaghJyr/kyoGp3OGEEN1YLtQZZdZXgPfSbXRsSDZ23JdXV7EdYG3ESK7samP6ExurG/MdtjFTOUe4xpYnPAno07v6cHlvl9D8Rlbm8+yn5p/79I6rQbnIpHyrGhReXXPZ7UD3M7UPin38qCOA3cj0O5Hbr/XRM6hfqeghFxVev32KmLH1ckRj01e+E9LTbY2Icm49h7pBEdHspy7NvbPpdhcrg3OtGkK0md1kHCHKg7B9x39BbAFjJFMR36fVNRXHZt6bee2LVTGe5nwb+nP5p698nvgtuT3POeYp/4nQ97DKw7Mev5jMh1HY83vWv+sAD+SGT0Afsv4ZQw9jn6fj825oL0RhPG77tzG0RGS39UYcU+96S6jntn8h6Feppu8RsVzd50jgYd+JtLrenFrmLsffC+Vebn/ewTancAtsLxJvpB5ywi70AUuz46sm+VPbP9nh1bSryF1QoDm9qz+JDxS7WBV1m2y7C9o6aZvKw7PM74i8WAVfwvaI3f1OZUFa/ZtR3pMG7ZLL40EJ6/orfij0AV5PfG5WmzlCaBP94nlkvGZn5BxQ1A4VanSP0p6dQt23G+E+Mr3LUUOk7Bog4kOZnJEUc/1UBbnDz2xqVnldGN7ZhKde68C8NLO8UqrB2RbyK4ztkE7r4n3w8oWXA5utlD2ochGDNhc0xrEJw7qitcG6Wq8pwDfavY9n5xH2q5O9BBFdvVdFcrXvfSHNevOdjDzflXffTTbmcAh9My8Rzq9S1uZAO/6OGiKZTZEcFJnL/aqoPW2Blcvr0XNkPDxTeZLZc3wXN1bJ6juIr8I7t0KQMc/Pa2PtqqwtYWOaG09Ls1iGN5qhkcGzR8UpJH8cq5TRu178YrqXJwD5qcOOBB5GTuUeJCY0+/zne9Y9J5K5zPpicWzh3aNiO1kbeY/D2ogPVRbkrfkdV4+sHY8SHsC9Zt37tRuHwLxZogvmm5JsnM7weXctxOtZLs5lzDZzd8BujFv7vhbiKQMfAbQkX+6i9iVJ7uqK1LxtKwsyvKchHugiF6OGujB3CJ9/5kCiq1mdj7TXTRfKsa+I7AUL5X7JT7j+TVtQx65/8/aRrK+k71imf6eK8PziMpiPNT7lrjUyt/fGuapR0TiMoplzf0l5P/zXkXxJDtEZlosJ28C82SUHI/H9IOGc60YRXk93JIRxjqngu0oIqO/pQl8rb9may0dEfW2Epws6Ic1V9O7U4Anv9uHVzcQG47v96+JCmjX5YRh3rrufK2vecLxXC0Pa1Hw0SzW3w5zCWpIngzLZNAnvT5THOrA1jObVjXGVsjUqr66++HTmDs5HGHoYP3h4BfXTh4+AcUQ4XpONJ5m894t1rl56FPIz9rboefaIbfKDM9ys/+3NPV/mtV4Rx3nqAemg8JrT6m2ib15ddF9ptN9mB9uSpVtFaF7rU2oOhICKjXPfljlHxfEQvWOb14ek3oPXQsu3xnlYpvIyl39243Q3KLiPu/OOI+sjiZ/2UxerIjxN9hz8zuYm15jOnhli9aNTZUFecpmwPb15KH5P0jhQaF+8nk/D/NCYVvH6Y8Jfoa76lxD/q7Wpwj6MwveJTit2qsUq+IuQZsWuUqGM8+9SkaQXmNwX3vMZRfNTmhNCsn0yOBRN1jtaukNz9Tv2uBRyncpDM5dtDOU55hf/nfSBfoc8bUuB9aAm0+/vRNtTbH88C7KrGrljM5UBTjegFNj4q5BMKquOgvYgKpOj1tF7bGSP24fe3JtZV5zL9T2ZDnw/Gf7L0LdzA4xLWrscWjX7zO4boY/1efDMXrgvOYdHbdjy6+KWeTo+r1s6O68OStLqrSEkcict1MGZRdiGtZe4QeGhye3hewdGxr03Q5oRUD4T74NS7MUoLpEbvhSvo8J+wvmoXy+kz1q3mP+dmuj8Hkgz9/Tyz5saL0rE5ynVPxL70DEwL292zmNw2XQYfp8S6EL4bgr14x1H7J80m7wn6kazrJ8Sx4CrVSEt9etVFuQn0tyxMflDqB+vXYl8clLe64j6sApmAPlLynO4+5uMwg2hfsSM5fR7a1bBYuaOwbO59Ek1+Ow70q3kgrzr0Kz/y727i7xLDeW59mQhT62K9/GZZUAHRLO+uSbNgswnxWcucCzL6OrdF6dDJGXVG1z6/OL14rWGfRD5acW+SoX0RWKBqjuAfZFmdwGdUexdgv2YOGcSzFF2zsk8mp0riH7opfR8GtatvhFamHzduKPSKG9K+iV7RUG/Ya4exzljuHyxLztKyGdj+RS8f7I4sp6J6kx78F6F9clFgC+ZDv7+P7vkMeKT7caH40ER9YHeGbI+qrqdWdvTfEK0qvVNf2A3LLPAYbFsP/9dXIb3LzA2q6BB5eFZFbvjtI+7+XcgtlfAHK1dtZnF0ktP/54Yroye46u9j8vTbKzIpU9uP4mJVcDaAvK5NZGzf0Xc71yw9wYgmgcsieh0RLd2ryHnGy3z3LjX2uSESMF5FlFVvfQKru3MFGM7ZDY9zo5BYvVDL50Qz8qLSvnc53tzjFkdyjsOWZ+Mv4U4YUNEjzCabtTGlbL/qLy65lx6X73edRrfD3o/ovnxEnEcqwzcDnPcNYtVHp69ZGSCQti4kaKFsTOPZr/yPPKOPpP6jWr71zkEqSb63RtGxgddzZ03xf6ExgK9+feHSc5u4svsxiaCY3hPkzhWWl0L5M/7pyykRTeu3sqCNPlelYcXvgPFPmqeA9uLxHCOlNv39SRXDvzUV1utgm9VXl1r5LH0buF9/G+sG4WbKS1b+PeFS87LQjupN38D8j5+7cdkbWDdRN8P+KmFzB2SK1SmiYjc574nfPUxQj7bVTiWRfZgcuRdArnfG59X9ZLBmV9a10uuBfsZzM7Rwhy32dkP+ADLnE377UrltZ1Z25ug42Vz2mx7HuZVLLCRr2Zl/gQGL+qPBmE5KPkyr92wl+xkdH8Gd84zlX3InOsGeH9B45fa4QkWpM9I5bdX7ZuBHIjyJUrZVhT7yA1ThTZHkmqBP5d1+i1yTHP+887BfSVIV/d+K4z/5IZvQR/o3mIOXrKN3AsFx7Ed5zHtqkiuXuBr/WJ6RyCLZ09h928wuZben7tBQeNobRLyStb/PpEF6V3EFnSjOWvzHSB/63wif8EdFPgeCn7drD8iNlHZ/Td+fK9OJu1pCzbHg8IbW+BLDU9Hc6Tv+jyvXJnTnqYxSMdv+f065wAfZby4G9hsqWaGUA7IucVu+nZNC9q/UX99PCi8usb0ITTeHd4F+wj1KCW4+86E5+2VVR9w+1DKeiW6AYP+3h+P99cZ0XMsFM3sk5OtkBaQ52ZjnwTAk7csB/zz6qJ3iDN7ZuUrnKDgGGl4ftsU5PFCuW98hkuL7b8L7LO+a4s4N6eX4T29HeI9hmafj4lMBucs8B2n/Pb0XW+4UK4qQl+Wdx+j3TgHzKXf8VNhH5YJ79UwlWDvM6bPMue4QeFV1WR8rxCm5dy8fM7aT+a7GaqQlkzxcWh/mNmJ8yByexzYRKnmLi4tfmUV9R90fxZA02eAc55QDvxL0L1+JA/LQWPOUYO6fvGH6J26PCF3D/L+XlB2YTwF6mpv4P2T3t783+eU9rwG5shPdcL1JbgrLNJH/RN0/5BOv3uNYpp5+WMGfE8ry4MFUK5SGnc+rB3vcm2Y7bepyBbLF6hB4dW1e28hc2lMV7hzKQ8q5farl+a+00DSYfTuCRyrfJt3vNaOR+leyc5pQp5Ks7dAvXwnJ1uhjK/plwYVQ38g6tf8B84XV08Wyp9uDFdZkO0XJnh4ip3i875qFurD2nFCXlrQvtswBqn/TGXQCJ7fYcQNrx75I2P6B2pQxGP7LbaJlYuFY7N2XALXdCr/jiNr38vcsVnmQBTDKTaINeh8gDsQ2DPkN7hBQXXLIvefhvcPdY1L4z4SKrSTPZ2wC7m9E9Zl7eTaQmAPjOVXC+2fVXBk+H1YNyg8NL1rFPEB9O4Rfnt+akw5lKE2cmY0OLOvvZwWy7hUiGPWJP2WYRTNVB4AsV5Sns2dN6WslOi07Vfnz7NO/zG1VwvZPDV7Z1zwTmT/RBwLaFwb4c0Rf0dCNx5X8byx73UgWrL+F4TPTF4DoCVngjOepLJx9UlpafX55Jxhdg0pzDPYVWfic7PyBNgH0I/h6RAiD7oyOl6MGO5rVv8FbFM2TgC+wq99QcgXL3kG8DPK7rcJD+T26B6giKz1u4usL/0U3e+M53w4HFt4b9QanKPxkifCGKTzAPXnK93vWVt9S8/Odea5U7Z0QgLKboUTIpkKWGdIwEZIv1K9npPxnsX6s7myJp1N7+fD7W2IvCuPyt9M6D+K3iExob5yZTwoorroToVQ/g7yzxgb058h/DPn5rn33C1OdAMGT82LJC6W9ZsdtqZfe0ka4AI5vQ3YcbJXRQpHl8Mzm8YHBelv6xuOBB/oj+ZewJw14n8r1EtuV0OI7UfvZ436iqYtxBZ/en5ChOennsd3aPSM3msfI9GF2Sf4hhzee26Zv1SDwmvOVOj+JuVQI5ShKO9X/4Hubyqke+Tw/ZMPorWIXsq/K9v2J+H3sM1LuXKqDAR7TCLjvQPIZHZtRmQy2MMN93FHeboH29NBz+aJn/E9DogHN0bu1wvvjcp94yaaI8syJfx2cNbeW8I+pKKvgfxp9UvjLJbh7WTHILEyqQx8xA0Kom9gNFaJ2uwnnf/K2sAY9SY+zzT31qw/J3fWSlA28PuKAGIV9BfnhxZ+DsfW1l9oI2TdVtGaRMc5HrPzNbi2t58DfJEHRdfJjE6bN5L9JWSPPUVDiNZH2E/75xSlYd2+Dtucn35aSLPe1QffJZrif2PTMnN7jDCQ1DU7bTcoiH+mkvf7zpr8axLb6sYafoxpvuLkIDFTyTghPFO5jNohG3zPMGsvr8oj9yrwv3qXeP1r+39zkJ6X4PvlbT8G4itN9lRu1ZqLqL1visb3jD8/5IZ4sXD9jtodcxXd59YqzkcEd5TgORom7sN2uPss/dQ7uT0dA1y5nX8OSO+6H8gps2ezOy3OQ3HI54tUiEXj+WC8ejPdQ6+1nQhigeCOzLCqYt+VV/eVWcNy+x3f569N9d4qAyR1/WI7rnfdknu3+HgefRubIBBnhxBPm1maYEFKNb3XOYb+W01iEGM5zZ3/dx60XvDWOZE7PQBfMil6F6GXzMYPHDSv9fg4heyPvl9FYL+jCeGYysUOkKuyC7k+T1lVQGTX9rfy5co4n95d1Ple1L4gRMPD74DsKniPg3IIns/zko/ksbvr0ZlRrS1yp1hODGy/VrVL1nLHYXgfOrSPW7i2yfZvY9Ykx0b6CMcKvodLKgd7EuT293Nr3ei3oVlSJoHxyjr4truU6bgX8i8arzXF0vT7jedmY6IuQrPh6cL9/F7dO/z5tR8hfJFqwDcxoPxVPuLie3uKEY5VsRLYXfrNFg7NygZwB6cmn55mscymQrCekfacCnkfpbnsV8yZ2zFCmg1vKIqbvNYoT1nZJd/cgfZPfQLZ00zxEW5QeHXDWBPbqzrhnCfrP+PmVfQZi5k7esZxZTKT6EHsld0oXmuYTROBvQp8HRdRwufogX3OvfsL7Jrt02/BJg/Op9/hpd/bAM0ZL54J+g3tSxTPj5UkugGlIJYenbvPaKnwzG10bx68j5D1jVcRXunGIVXUnt14a9geAloF+P5NecIAfD4gdUdeH+X1/AnhS6YD3H9KfhnecSoL8is3sjFNzOxzLKmn1a8A86HVn0p0WpP7Rn1LRLdcUFd0X1Xy5RvxXYmtY92g8JrTZsJvYyllW7l8lhvOI3xWyh7My6uk/G+V3sVQ7sqHZVJXbtfEdjL8dm4OZEz/KZcWcqYhFox9Stwy2T0YkBarYG2494i/fiuLcefSqugk82GZ0T2VEXkuXURsk+2Lc5Hm3ODsHysvv4P7FJTXHaZiTG6vIDbR9uH96NnYnWvb9eZTE7S9P3PnPCl/7oR9eHUrGRz6jVw/MSHN1kzK0e98M/YqfFcSzpt3AHwnxFvTbT8NqWxOOy4dFISXPEOcV/HKYczw0jZAn91oRuuSyl6r50pFx9Bvr8z9PVeubH8+6MNecmqCxYLfj4Z1NVkJ6yIgvkcG7ZfPfdvuJkcSVUY0XsCcld7LpU8v/S2N9WpGceVZb65n69LKgZ+vwd+Akpxj8toXedCDdH/JgOvTPLTsOgrHwiRzA94LxTKpAWQ+ZP1OaOvk44G8WObnqpAWvQXnCcc8BmWIyPgd3PEq9mMtQcF+Wvqe8W5l7gpjcVmd2aOGEGtH5ExNjxAR80BvfgmMV+86g2ufrR30DiG5YRmQZ7TXmkOxte8klj7a3uyBWT3S0z0YOhj6JKk7HNCC1gIhLRP435PLxtChTcz+96BDxmfcp1LdZ/d9Mf6j/WIiu37xAbEPcMYnugODNcn0NL7H5q+ql9zEjUu85C6ov1UDuHNpT7ma24ckvU3loIm+B8IxcAqNy+ws49a1KvDZ5mSbz9VLr+c6ri2WpBcIT+3GVaSuuewq5rulQ4FsKIdYGYDtyUtXprsBJWSD8Ro7eVpeXfCLL0uQtVXVSfGghEhm09XCefN6HpeAoFeFftCv3B7H3y+71kGAmhNBXa3+aa7984tfI7R02/dP/Dn9hgUe70VRm00atBvHpoNCZfdN/nmInEwjP+P/im93S+8VjlepPgDWv+bcfwD7F/rjaFXd+FhlQabykMpgxbyer5Gxmsox5ZBXNxNeGdN/yKXZ9vdHeLUgb1yXifUk61CzM7u+qZlKYiTdiOZDGH1r3Av00k/rwK+i7wfButR3Jx1V1vs5ZH4HjuPqtN58WSKEmAo4i0jQ5IY+oF+z6cIozVQ2/GPj6C7t0O5WDGZxAV+M6a/ERfsjJOlj4R2NdtVm1ypYQOyuZd7Klfvwe+yYV0vzzlF2Tch8G+FYLq+UsinMWuN4VdSc3/Fv/hq2dCzdc2y+AexkGJtn42CunTTmTSV1jReHCXmqdy3OrRHnwfH2CNFKJ4W512hds+kFmr8quYbry4gMhjLZMYHBg+3ZvYck0Nhab8Q55KqLQV1rZIzL58yXDxGZlFaze0somjKrgoH3YPotneQlP1HRHC2I2vuIXnYcleD6/dD3JO9XeVU1+Q2H217xEXG8D2VOHH0DkVOXfIcilLW5BXwepH4A+Vw8mosnN9wV8irmF9Mze1r9j9z/Uvxi9yG6KtWcJpxLU+ksRxro7HYl9WyubGjyBcR22j57N0339vDeCxb0IFxP25scUV1Jbcd3MJV+IFz3WOZRajegFLzH/Jr5bsFgNs+Q1W9wbg5U9ZIH4fmFMOeNgQRRL30rnqm8P/dd1cao/0B4aK2JnilIV5Xq9VE7Tn5l7Uwc1sV3fntJNYFtdnAfC6hLZj0p/9tBcx82p/LvaNQNEktlcd7k0qzYZ6B+DK/dhSzFdx7i9ynnxBX7PJWpTBBNZbcTvI8JAFbBi/n9ZWoAWAcohzq59kqSbsL7zLedlwbgMA7bdmQiKLy6ckMDVz9Qe0vmwfbAerA163uOJLGPZYLv34I+gvMUAcAvnhflKX4cdj8J60Nvc4OC9Wi+OHZMnqayIHgfPMUzO/F9NGhvQMWwRFBENPvF07D/ODCQa3ftKfeBWEA5NBjigTl/oEU3TqBnI1cZwn7xHRusTI7gzrkx/d/43Z3+nhMUXnPG8jXAlqA4SNCv4d1D794G8R7j84qfpnat6gbuXGptG9m6pLLhHZel8zXsj+S0GqmL8Lxkf4f+ZWx77puO1B8NYu0V+HaXXpqGeercPUn4/ILrBkXEgyDWwu/o8T1asv4jbo5MHlTLrPPo/VfZujHbfxDnhfWbW9B3P8M5L7uayxfFXqwGBQOKuHyRpNvKc3wBYzOV36gsdboBvlces8xFTvhvq6IxkvMdQWwduocD9pvDuUT4rhe/h2dsk72M2k5pBBwHjEWBHVcO/eAPaGytc3Ln938t1HOlrJCrC5rcw8Hno65zg8Kra/vvgu/jKGUjKK+ytAS5/zD/r3fdD2y7VTAuJ/exLE9z7xGca+PodygHzL4EfFfJg25QGFoYu3afGxT0rqg2lg4Kj+YwZ4rnaJ0K+RKJd43haZozn5yNu+53QiSlWvxNM715J9Fpq2IzV6eVsonccWRjCzcoCFCT35chGx3KaSmM+fVSfC7G9l9VeXWD3BQax8BI7iGU06qTuHNO7oAh8hzcR0exdCMm5kupF66xETC4exj9LT414vf3k/fYckP0zjjWFsP7huFel/+3z/8BPS9H1Py3AAA="
            4
    let scoring = createAccuracyMetric (SCPlus 4)
    scoring.ProcessAll(data)
    scoring.Value