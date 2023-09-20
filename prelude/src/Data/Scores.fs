namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Gameplay
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
    }

[<Json.AutoCodec(false)>]
type ChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<Score>
        PersonalBests: Dictionary<string, Bests>
        mutable LastPlayed: DateTime
        mutable Comment: string
    }
    static member FromChart(c: Chart) =
        {
            Offset = c.FirstNote
            Scores = List<Score>()
            PersonalBests = Dictionary<string, Bests>()
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

    member val Player : string option = None with get, set

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

    member this.Accuracy = this.Scoring.State.PointsScored / this.Scoring.State.MaxPointsScored

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
                (fun () -> score.selectedMods |> ModState.enumerate |> Seq.map (fun (_, m, _) -> m.Status) |> List.ofSeq |> fun l -> ModStatus.Ranked :: l |> List.max)
                modstatus |> ValueSome
        modstatus.Value

[<Json.AutoCodec>]
type ImprovementFlags =
    {
        Lamp: Improvement<int>
        Accuracy: Improvement<float>
        Grade: Improvement<int>
    }
    static member Default = 
        {
            Lamp = Improvement.None
            Accuracy = Improvement.None
            Grade = Improvement.None
        }

module Bests =

    let update (score: ScoreInfoProvider) (existing: Bests) : Bests * ImprovementFlags =
        let l, lp = PersonalBests.update (score.Lamp, score.ScoreInfo.rate) existing.Lamp
        let a, ap = PersonalBests.update (score.Scoring.Value, score.ScoreInfo.rate) existing.Accuracy
        let g, gp = PersonalBests.update (score.Grade, score.ScoreInfo.rate) existing.Grade
        
        { Lamp = l; Accuracy = a; Grade = g },
        { Lamp = lp; Accuracy = ap; Grade = gp }

    let create (score: ScoreInfoProvider) : Bests =
        {
            Lamp = PersonalBests.create (score.Lamp, score.ScoreInfo.rate)
            Accuracy = PersonalBests.create (score.Scoring.Value, score.ScoreInfo.rate)
            Grade = PersonalBests.create (score.Grade, score.ScoreInfo.rate)
        }

module Scores =

    [<Json.AutoCodec(false)>]
    type Data =
        {
            Entries: Dictionary<string, ChartSaveData>
        }
        static member Default =
            {
                Entries = new Dictionary<string, ChartSaveData>()
            }

    let data : Data =
        loadImportantJsonFile "Scores" (Path.Combine (getDataPath "Data", "scores.json")) true
        |> fun d -> Logging.Info (sprintf "Loaded scores for %i charts." d.Entries.Keys.Count); d

    let save() = saveImportantJsonFile (Path.Combine(getDataPath "Data", "scores.json")) data

    let getOrCreateData (chart: Chart) =
        let hash = Chart.hash chart
        if hash |> data.Entries.ContainsKey |> not then data.Entries.Add(hash, ChartSaveData.FromChart chart)
        data.Entries.[hash]

    let getData (hash: string) =
        if hash |> data.Entries.ContainsKey |> not then None else Some data.Entries.[hash]

    let saveScore (d: ChartSaveData) (score: Score) =
        d.Scores.Add score
        save()

    let saveScoreWithPbs (d: ChartSaveData) (rulesetId: string) (score: ScoreInfoProvider) : ImprovementFlags =
        saveScore d score.ScoreInfo

        if d.PersonalBests.ContainsKey rulesetId then
            let newBests, flags = Bests.update score d.PersonalBests.[rulesetId]
            d.PersonalBests.[rulesetId] <- newBests
            flags
        else
            d.PersonalBests.Add(rulesetId, Bests.create score)
            { Lamp = Improvement.New; Accuracy = Improvement.New; Grade = Improvement.New }