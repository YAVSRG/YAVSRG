namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout

[<Json.AutoCodec>]
type Score =
    {
        time: DateTime
        replay: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }

[<Json.AutoCodec>]
type Bests =
    {
        Lamp: PersonalBests<int>
        Accuracy: PersonalBests<float>
        Grade: PersonalBests<int>
        Clear: PersonalBests<bool>
    }

[<Json.AutoCodec(false)>]
type ChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<Score>
        Bests: Dictionary<string, Bests>
        mutable LastPlayed: DateTime
        mutable Comment: string
    }
    static member FromChart(c: Chart) =
        {
            Offset = c.FirstNote
            Scores = List<Score>()
            Bests = Dictionary<string, Bests>()
            LastPlayed = DateTime.UnixEpoch
            Comment = ""
        }

(*
    Gameplay pipelines that need to happen to play a chart
    Chart -> Modified chart -> Colorized chart
                            -> Replay data -> Mod replay data
                            -> Difficulty rating data
*)

type ScoreInfoProvider(score: Score, chart: Chart, ruleset: Ruleset) =
    let mutable ruleset: Ruleset = ruleset

    let mutable modchart = ValueNone
    let mutable modstring = ValueNone
    let mutable modstatus = ValueNone
    let mutable difficulty = ValueNone
    let mutable metrics = ValueNone
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

    member this.Ruleset
        with get() = ruleset
        and set(value) = if value <> ruleset then ruleset <- value; metrics <- ValueNone; lamp <- ValueNone; grade <- ValueNone
    member this.Scoring =
        metrics <-
            ValueOption.defaultWith (fun () -> 
                let m = Metrics.createScoreMetric ruleset this.ModChart.Keys (StoredReplayProvider this.ReplayData) this.ModChart.Notes score.rate 
                m.Update Time.infinity; m)
                metrics
            |> ValueSome
        metrics.Value

    member this.HP = this.Scoring.HP

    member this.Difficulty
        with get() =
            difficulty <- ValueOption.defaultWith (fun () -> RatingReport (this.ModChart.Notes, score.rate, score.layout, this.ModChart.Keys)) difficulty |> ValueSome
            difficulty.Value
        and set(value) = difficulty <- ValueSome value

    member this.Lamp =
        lamp <- ValueOption.defaultWith (fun () -> Lamp.calculate ruleset.Grading.Lamps this.Scoring.State) lamp |> ValueSome
        lamp.Value

    member this.Grade =
        grade <- ValueOption.defaultWith (fun () -> Grade.calculate ruleset.Grading.Grades this.Scoring.State) grade |> ValueSome
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

(*
    !! Score buckets are likely getting binned in favour of tables, in the near future !!
*)

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ScoreFilter =
    | Keymode of int
    | Playstyle of Layout
    | Grade of int
    | Lamp of int
    // pattern stuff
    
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ScoreSort =
    | Physical
    // pattern stuff
    
[<Json.AutoCodec>]
type TopScore =
    {
        Hash: string
        Timestamp: DateTime
        Physical: float
        // other stuff for sorting in future
    }

[<Json.AutoCodec>]
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

[<Json.AutoCodec>]
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

module Bests =

    let update (score: ScoreInfoProvider) (existing: Bests) : Bests * BestFlags =
        let l, lp = PersonalBests.update (score.Lamp, score.ScoreInfo.rate) existing.Lamp
        let a, ap = PersonalBests.update (score.Scoring.Value, score.ScoreInfo.rate) existing.Accuracy
        let g, gp = PersonalBests.update (score.Grade, score.ScoreInfo.rate) existing.Grade
        let c, cp = PersonalBests.update (not score.HP.Failed, score.ScoreInfo.rate) existing.Clear
        
        { Lamp = l; Accuracy = a; Grade = g; Clear = c },
        { Lamp = lp; Accuracy = ap; Grade = gp; Clear = cp }

    let create (score: ScoreInfoProvider) : Bests =
        {
            Lamp = PersonalBests.create (score.Lamp, score.ScoreInfo.rate)
            Accuracy = PersonalBests.create (score.Scoring.Value, score.ScoreInfo.rate)
            Grade = PersonalBests.create (score.Grade, score.ScoreInfo.rate)
            Clear = PersonalBests.create (not score.HP.Failed, score.ScoreInfo.rate)
        }

module Scores =

    [<Json.AutoCodec(false)>]
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
        loadImportantJsonFile "Scores" (Path.Combine (getDataPath "Data", "scores.json")) true
        |> fun d -> Logging.Info (sprintf "Scores loaded, %i chart entries and %i buckets." d.Entries.Keys.Count d.Buckets.Keys.Count); d

    let save() = saveImportantJsonFile (Path.Combine(getDataPath "Data", "scores.json")) data

    let getOrCreateData (chart: Chart) =
        let hash = Chart.hash chart
        if hash |> data.Entries.ContainsKey |> not then data.Entries.Add(hash, ChartSaveData.FromChart chart)
        data.Entries.[hash]

    let getData (hash: string) =
        if hash |> data.Entries.ContainsKey |> not then None else Some data.Entries.[hash]

    let saveScore (d: ChartSaveData) (score: ScoreInfoProvider) =
        d.Scores.Add score.ScoreInfo
        save()

    let saveScoreWithPbs (d: ChartSaveData) (rulesetId: string) (score: ScoreInfoProvider) : BestFlags =
        saveScore d score

        if d.Bests.ContainsKey rulesetId then
            let newBests, flags = Bests.update score d.Bests.[rulesetId]
            d.Bests.[rulesetId] <- newBests
            flags
        else
            d.Bests.Add(rulesetId, Bests.create score)
            { Lamp = PersonalBestType.Faster; Accuracy = PersonalBestType.Faster; Grade = PersonalBestType.Faster; Clear = PersonalBestType.Faster }