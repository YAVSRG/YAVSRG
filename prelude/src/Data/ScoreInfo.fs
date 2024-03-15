namespace Prelude.Data

open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Data.Library.Caching

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

        Rating: DifficultyRating
        Physical: float

        ImportedFromOsu: bool
    }
    member this.Ruleset
        with get () = this.Scoring.Ruleset
        and set (ruleset) =
            let scoring =
                Metrics.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate

            this.Scoring <- scoring
            this.Lamp <- Lamp.calculate ruleset.Grading.Lamps scoring.State
            this.Grade <- Grade.calculate ruleset.Grading.Grades scoring.State

    member this.Accuracy = this.Scoring.Value
    member this.Mods = this.WithMods.ModsApplied

    member this.ModStatus() =
        match Mods.check this.Mods with
        | Ok r -> r
        | Error msg -> failwith msg

    member this.ModString() =
        Mods.format_mods (this.Rate, this.Mods, false)

module ScoreInfo =

    let from_score (cc: CachedChart) (chart: Chart) (ruleset: Ruleset) (score: Score) : ScoreInfo =
        let with_mods = apply_mods score.Mods chart
        let replay_data = score.Replay |> Replay.decompress_bytes

        let scoring =
            Metrics.run ruleset with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.Rate

        let difficulty = DifficultyRating.calculate score.Rate with_mods.Notes

        {
            CachedChart = cc
            Chart = chart
            WithMods = with_mods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = score.Timestamp
            Rate = score.Rate

            Replay = replay_data
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Grading.Lamps scoring.State
            Grade = Grade.calculate ruleset.Grading.Grades scoring.State

            Rating = difficulty
            Physical = Performance.calculate difficulty with_mods.Keys scoring |> fst

            ImportedFromOsu = score.IsImported
        }

    let to_score (score_info: ScoreInfo) : Score =
        {
            Timestamp = score_info.TimePlayed
            Replay = score_info.Replay |> Replay.compress_bytes
            Rate = score_info.Rate
            Mods = score_info.Mods
            IsImported = score_info.ImportedFromOsu
            Keys = score_info.WithMods.Keys
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
