namespace Prelude.Data.Scores

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Tools
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Gameplay.Layout
open Prelude.Data.Charts.Caching

[<Json.AutoCodec(true)>]
type Score =
    {
        time: DateTime
        replay: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }
    static member Default = { time = Unchecked.defaultof<_>; replay = ""; rate = 1.0f; selectedMods = Map.empty; layout = Layout.Spread; keycount = 4 }

[<Json.AutoCodec(true)>]
type Bests =
    {
        Lamp: PersonalBests<int>
        Accuracy: PersonalBests<float>
        Grade: PersonalBests<int>
    }
    static member Default = { Lamp = []; Accuracy = []; Grade = [] }

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
    static member Default = { Offset = 0.0f<ms>; Scores = List<Score>(); PersonalBests = Dictionary<string, Bests>(); LastPlayed = DateTime.UnixEpoch; Comment = "" }
    static member FromChart(c: Chart) =
        {
            Offset = c.FirstNote
            Scores = List<Score>()
            PersonalBests = Dictionary<string, Bests>()
            LastPlayed = DateTime.UnixEpoch
            Comment = ""
        }



[<RequireQualifiedAccess>]
type ScorePlayedBy =
    | You
    | Username of string

// Everything you need to display a score screen or watch a replay of a score
type ScoreInfo =
    {
        CachedChart: CachedChart
        Chart: Chart
        WithMods: ModdedChart

        PlayedBy: ScorePlayedBy
        TimePlayed: int64
        Rate: float32

        Replay: ReplayData
        mutable Scoring: ScoreMetric
        mutable Lamp: int
        mutable Grade: int

        Rating: RatingReport
        Physical: float

        ImportedFromOsu: bool
    }
    member this.Ruleset 
        with get () = this.Scoring.Ruleset
        and set (ruleset) =
            let scoring = Metrics.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate
            this.Scoring <- scoring
            this.Lamp <- Lamp.calculate ruleset.Grading.Lamps scoring.State
            this.Grade <- Grade.calculate ruleset.Grading.Grades scoring.State
    member this.Accuracy = this.Scoring.Value
    member this.Mods = this.WithMods.ModsApplied

    member this.ModStatus () = match Mods.check this.Mods with Ok r -> r | Error msg -> failwith msg
    member this.ModString () = Mods.format_mods (this.Rate, this.Mods, false)

module ScoreInfo =

    let from_score (cc: CachedChart) (chart: Chart) (ruleset: Ruleset) (score: Score) : ScoreInfo =
        let with_mods = apply_mods score.selectedMods chart
        let replay_data = score.replay |> Replay.decompress_string
        let scoring = Metrics.run ruleset with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.rate
        let difficulty = RatingReport(with_mods.Notes, score.rate, with_mods.Keys)
        {
            CachedChart = cc
            Chart = chart
            WithMods = with_mods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = score.time |> Timestamp.from_datetime
            Rate = score.rate

            Replay = replay_data
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Grading.Lamps scoring.State
            Grade = Grade.calculate ruleset.Grading.Grades scoring.State

            Rating = difficulty
            Physical = calculate_score_rating difficulty with_mods.Keys scoring |> fst

            ImportedFromOsu = score.layout = Layout.Layout.LeftTwo
        }

    let to_score (score_info: ScoreInfo) =
        {
            time = score_info.TimePlayed |> Timestamp.to_datetime
            replay = score_info.Replay |> Replay.compress_string
            rate = score_info.Rate
            selectedMods = score_info.Mods
            layout = Layout.Layout.Spread
            keycount = score_info.WithMods.Keys
        }

module Bests =

    let update (score_info: ScoreInfo) (existing: Bests) : Bests * ImprovementFlags =
        let l, lp = PersonalBests.update (score_info.Lamp, score_info.Rate) existing.Lamp

        let a, ap =
            PersonalBests.update (score_info.Accuracy, score_info.Rate) existing.Accuracy

        let g, gp = PersonalBests.update (score_info.Grade, score_info.Rate) existing.Grade

        { Lamp = l; Accuracy = a; Grade = g }, { Lamp = lp; Accuracy = ap; Grade = gp }

    let create (score_info: ScoreInfo) : Bests =
        {
            Lamp = PersonalBests.create (score_info.Lamp, score_info.Rate)
            Accuracy = PersonalBests.create (score_info.Accuracy, score_info.Rate)
            Grade = PersonalBests.create (score_info.Grade, score_info.Rate)
        }

module Scores =

    [<Json.AutoCodec(false)>]
    type Data =
        {
            Entries: ConcurrentDictionary<string, ChartSaveData>
        }
        static member Default =
            {
                Entries = new ConcurrentDictionary<string, ChartSaveData>()
            }

    // todo: move this to an SQLite database
    let mutable data : Data = Unchecked.defaultof<_>

    let save () = save_important_json_file (Path.Combine(get_game_folder "Data", "scores.json")) data

    let init_startup() =
        data <-
            load_important_json_file "Scores" (Path.Combine(get_game_folder "Data", "scores.json")) true
            |> fun d ->
                Logging.Info(sprintf "Loaded scores for %i charts." d.Entries.Keys.Count)
                d

    let deinit () = save()

    let get_or_create (chart: Chart) =
        let hash = Chart.hash chart

        if not (data.Entries.ContainsKey hash) then
            data.Entries.[hash] <- ChartSaveData.FromChart chart

        data.Entries.[hash]

    let get (hash: string) =
        if hash |> data.Entries.ContainsKey |> not then
            None
        else
            Some data.Entries.[hash]

    let save_score (d: ChartSaveData) (score_info: ScoreInfo) =
        d.Scores.Add (ScoreInfo.to_score score_info)
        save ()

    let save_score_pbs (d: ChartSaveData) (ruleset_id: string) (score_info: ScoreInfo) : ImprovementFlags =
        save_score d score_info

        if d.PersonalBests.ContainsKey ruleset_id then
            let newBests, flags = Bests.update score_info d.PersonalBests.[ruleset_id]
            d.PersonalBests.[ruleset_id] <- newBests
            flags
        else
            d.PersonalBests.Add(ruleset_id, Bests.create score_info)

            {
                Lamp = Improvement.New
                Accuracy = Improvement.New
                Grade = Improvement.New
            }

    let save_comment (d: ChartSaveData) (comment: string) =
        d.Comment <- comment
        
    let save_offset (d: ChartSaveData) (offset: Time) =
        d.Offset <- offset
    
    let get_best_grade_above (ruleset_id: string) (rate: float32) (chart_hash: string) =
        match get chart_hash with
        | Some d ->
            if d.PersonalBests.ContainsKey ruleset_id then
                PersonalBests.get_best_above rate d.PersonalBests.[ruleset_id].Grade
            else
                None
        | None -> None