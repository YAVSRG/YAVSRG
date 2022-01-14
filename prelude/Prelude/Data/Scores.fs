namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout

[<Json.AllRequired>]
type Score =
    {
        time: DateTime
        replay: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }
    //only used for Percyqaz.Json performance
    static member Default =
        { 
            time = DateTime.Now
            replay = ""
            rate = 1.0f
            selectedMods = Map.empty
            layout = Layout.Spread
            keycount = 4
        }

type Bests =
    {
        Lamp: PersonalBests<int>
        Accuracy: PersonalBests<float>
        Grade: PersonalBests<int>
        Clear: PersonalBests<bool>
    }

type ChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<Score>
        Bests: Dictionary<string, Bests>
    }
    static member FromChart(c: Chart) =
        {
            Offset = c.FirstNote
            Scores = List<Score>()
            Bests = Dictionary<string, Bests>()
        }
    static member Default =
        {
            Offset = 0.0f<ms>
            Scores = null
            Bests = Dictionary<string, Bests>()
        }

(*
    Gameplay pipelines that need to happen to play a chart
    Chart -> Modified chart -> Colorized chart
                            -> Replay data -> Mod replay data
                            -> Difficulty rating data
*)

type ScoreInfoProvider(score: Score, chart: Chart, scoring: ScoreSystemConfig) =
    let mutable scoringConfig: ScoreSystemConfig = scoring

    let mutable modchart = ValueNone
    let mutable modstring = ValueNone
    let mutable modstatus = ValueNone
    let mutable difficulty = ValueNone
    let mutable scoreMetric = ValueNone
    let mutable replayData = ValueNone
    let mutable perf = ValueNone
    let mutable lamp = ValueNone
    let mutable grade = ValueNone

    member this.ScoreInfo = score
    member this.Chart = chart

    member this.ReplayData =
        replayData <- ValueOption.defaultWith (fun () -> Replay.decompress score.replay) replayData |> ValueSome
        replayData.Value

    member this.ModChart
        with get() =
            modchart <- ValueOption.defaultWith (fun () -> getModChart score.selectedMods chart) modchart |> ValueSome
            modchart.Value
        and set(value) = modchart <- ValueSome value

    member this.ScoringConfig with get() = scoringConfig and set(value) = if value <> this.ScoringConfig then scoringConfig <- value; scoreMetric <- ValueNone; lamp <- ValueNone
    member this.Scoring =
        scoreMetric <-
            ValueOption.defaultWith (fun () -> 
                let m = Metrics.createScoreMetric (Metrics.AccuracySystemConfig.Custom scoringConfig) this.ModChart.Keys (StoredReplayProvider this.ReplayData) this.ModChart.Notes score.rate 
                m.Update Time.infinity; m)
                scoreMetric
            |> ValueSome
        scoreMetric.Value

    member this.HP = this.Scoring.HP

    member this.Difficulty
        with get() =
            difficulty <- ValueOption.defaultWith (fun () -> RatingReport (this.ModChart.Notes, score.rate, score.layout, this.ModChart.Keys)) difficulty |> ValueSome
            difficulty.Value
        and set(value) = difficulty <- ValueSome value

    member this.Lamp =
        lamp <- ValueOption.defaultWith (fun () -> Lamp.calculate scoring.Grading.Lamps this.Scoring.State) lamp |> ValueSome
        lamp.Value

    member this.Grade =
        grade <- ValueOption.defaultWith (fun () -> Grade.calculate scoring.Grading.Grades this.Scoring.State) grade |> ValueSome
        grade.Value

    member this.Physical =
        perf <- ValueOption.defaultWith (fun () -> getRatings this.Difficulty score.keycount this.Scoring) perf |> ValueSome
        fst perf.Value
    member this.Technical =
        perf <- ValueOption.defaultWith (fun () -> getRatings this.Difficulty score.keycount this.Scoring) perf |> ValueSome
        snd perf.Value

    member this.Mods =
        modstring <-
            ValueOption.defaultWith
                (fun () -> getModString(score.rate, score.selectedMods, false))
                modstring |> ValueSome
        modstring.Value

    member this.ModStatus =
        modstatus <-
            ValueOption.defaultWith
                (fun () -> score.selectedMods |> ModState.enumerate |> List.ofSeq |> List.map (fun s -> modList.[s].Status) |> fun l -> ModStatus.Ranked :: l |> List.max)
                modstatus |> ValueSome
        modstatus.Value

[<RequireQualifiedAccess>]
type ScoreFilter =
    | Keymode of int
    | Playstyle of Layout
    | Grade of int
    | Lamp of int
    // pattern stuff
    
[<RequireQualifiedAccess>]
type ScoreSort =
    | Physical
    // pattern stuff

type TopScore =
    {
        Hash: string
        Timestamp: DateTime
        Physical: float
        // other stuff for sorting in future
    }

type ScoreBucket =
    {
        Filters: ScoreFilter list
        Sort: ScoreSort
        Scores: ResizeArray<TopScore>
        Size: int
    }
    static member Default =
        {
            Filters = [ScoreFilter.Keymode 4]
            Sort = ScoreSort.Physical
            Scores = ResizeArray()
            Size = 50
        }
module Bucket =
    
    let add (score: ScoreInfoProvider) (bucket: ScoreBucket) =
        if 
            List.forall
                (
                    function
                    | ScoreFilter.Keymode k -> score.ScoreInfo.keycount = k
                    | ScoreFilter.Playstyle p -> score.ScoreInfo.layout = p
                    | ScoreFilter.Grade g -> score.Grade >= g
                    | ScoreFilter.Lamp l -> score.Lamp >= l
                )
                bucket.Filters
        then
            let sort =
                match bucket.Sort with
                | ScoreSort.Physical -> fun a b -> b.Physical.CompareTo a.Physical
            let topScore =
                {
                    Hash = score.Chart |> Chart.hash
                    Timestamp = score.ScoreInfo.time
                    Physical = score.Physical
                }
            if
                match bucket.Scores |> Seq.tryFind (fun x -> x.Hash = topScore.Hash) with
                | Some existingScore when sort existingScore topScore <= 0 -> bucket.Scores.Remove existingScore // evaluates true
                | Some existingScore -> false
                | None -> true
            then
                bucket.Scores.Add topScore
                bucket.Scores.Sort sort
                if bucket.Scores.Count > bucket.Size then bucket.Scores.RemoveAt bucket.Size

type BestFlags =
    {
        // future: marker of if you beat a bucket score/goals achieved etc
        Lamp: PersonalBestType
        Accuracy: PersonalBestType
        Grade: PersonalBestType
        Clear: PersonalBestType
    }
    static member Default = 
        {
            Lamp = PersonalBestType.None
            Accuracy = PersonalBestType.None
            Grade = PersonalBestType.None
            Clear = PersonalBestType.None
        }

module Scores =

    type Data =
        {
            Entries: Dictionary<string, ChartSaveData>
            Buckets: Dictionary<string, ScoreBucket>
        }
        static member Default =
            {
                Entries = new Dictionary<string, ChartSaveData>()
                Buckets = [("Best 4k", ScoreBucket.Default)] |> Map.ofList |> Dictionary<string, ScoreBucket>
            }

    let data: Data =
        loadImportantJsonFile "Scores" (Path.Combine (getDataPath "Data", "scores.json")) Data.Default true
        |> fun d -> Logging.Info (sprintf "Scores loaded, %i chart entries and %i buckets." d.Entries.Keys.Count d.Buckets.Keys.Count); d

    let save() = saveImportantJsonFile (Path.Combine(getDataPath "Data", "scores.json")) data

    let getOrCreateScoreData (chart: Chart) =
        let hash = Chart.hash chart
        if hash |> data.Entries.ContainsKey |> not then data.Entries.Add(hash, ChartSaveData.FromChart chart)
        data.Entries.[hash]

    let getScoreData (hash: string) =
        if hash |> data.Entries.ContainsKey |> not then None else Some data.Entries.[hash]

    let saveScore (d: ChartSaveData) (score: ScoreInfoProvider) : BestFlags =
        d.Scores.Add score.ScoreInfo
        // update score buckets
        for b in data.Buckets.Values do Bucket.add score b
        save()
        // update pbs
        if d.Bests.ContainsKey score.Scoring.Name then
            let existing = d.Bests.[score.Scoring.Name]
            let l, lp = PersonalBests.update (score.Lamp, score.ScoreInfo.rate) existing.Lamp
            let a, ap = PersonalBests.update (score.Scoring.Value, score.ScoreInfo.rate) existing.Accuracy
            let g, gp = PersonalBests.update (score.Grade, score.ScoreInfo.rate) existing.Grade
            let c, cp = PersonalBests.update (not score.HP.Failed, score.ScoreInfo.rate) existing.Clear
            d.Bests.[score.Scoring.Name] <-
                {
                    Lamp = l
                    Accuracy = a
                    Grade = g
                    Clear = c
                }
            { Lamp = lp; Accuracy = ap; Grade = gp; Clear = cp }
        else
            d.Bests.Add(
                score.Scoring.Name,
                {
                    Lamp = PersonalBests.create (score.Lamp, score.ScoreInfo.rate)
                    Accuracy = PersonalBests.create (score.Scoring.Value, score.ScoreInfo.rate)
                    Grade = PersonalBests.create (score.Grade, score.ScoreInfo.rate)
                    Clear = PersonalBests.create (not score.HP.Failed, score.ScoreInfo.rate)
                }
            )
            { Lamp = PersonalBestType.Faster; Accuracy = PersonalBestType.Faster; Grade = PersonalBestType.Faster; Clear = PersonalBestType.Faster }

    //todo: background tasks to reprocess buckets from score database