namespace Prelude.Data.User

open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Difficulty
open Prelude.Charts.Processing.Patterns
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Data.Library

[<RequireQualifiedAccess>]
type ScorePlayedBy =
    | You
    | Username of string

// Everything you need to display a score screen or watch a replay of a score
type ScoreInfo =
    {
        ChartMeta: ChartMeta
        Chart: Chart
        WithMods: ModdedChart

        PlayedBy: ScorePlayedBy
        TimePlayed: int64
        Rate: float32

        Replay: ReplayData
        mutable Scoring: ScoreProcessor
        mutable Lamp: int
        mutable Grade: int

        Rating: DifficultyRating
        Patterns: PatternInfo
        Physical: float

        ImportedFromOsu: bool
    }
    member this.Ruleset
        with get () = this.Scoring.Ruleset
        and set (ruleset) =
            let scoring =
                ScoreProcessor.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate

            this.Scoring <- scoring
            this.Lamp <- Lamp.calculate ruleset.Grading.Lamps scoring.State
            this.Grade <- Grade.calculate ruleset.Grading.Grades scoring.State
    
    member this.WithRuleset (ruleset: Ruleset) =
        let scoring =
            ScoreProcessor.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate

        { this with
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Grading.Lamps scoring.State
            Grade = Grade.calculate ruleset.Grading.Grades scoring.State
        }

    member this.Accuracy = this.Scoring.Accuracy
    member this.Mods = this.WithMods.ModsApplied

    member this.ModStatus = this.WithMods.Status

    member this.ModString() =
        Mods.format (this.Rate, this.Mods, false)

module ScoreInfo =

    let from_score (cc: ChartMeta) (chart: Chart) (ruleset: Ruleset) (score: Score) : ScoreInfo =
        let with_mods = Mods.apply score.Mods chart
        let replay_data = score.Replay |> Replay.decompress_bytes

        let scoring =
            ScoreProcessor.run ruleset with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.Rate

        let difficulty = DifficultyRating.calculate score.Rate with_mods.Notes
        let patterns = PatternSummary.generate_pattern_data score.Rate chart

        {
            ChartMeta = cc
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
            Patterns = patterns
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
        let l, lp = PersonalBests.update (score_info.Lamp, score_info.Rate, score_info.TimePlayed) existing.Lamp

        let a, ap =
            PersonalBests.update (score_info.Accuracy, score_info.Rate, score_info.TimePlayed) existing.Accuracy

        let g, gp = PersonalBests.update (score_info.Grade, score_info.Rate, score_info.TimePlayed) existing.Grade

        { Lamp = l; Accuracy = a; Grade = g }, { Lamp = lp; Accuracy = ap; Grade = gp }

    let create (score_info: ScoreInfo) : Bests =
        {
            Lamp = PersonalBests.create (score_info.Lamp, score_info.Rate, score_info.TimePlayed)
            Accuracy = PersonalBests.create (score_info.Accuracy, score_info.Rate, score_info.TimePlayed)
            Grade = PersonalBests.create (score_info.Grade, score_info.Rate, score_info.TimePlayed)
        }
