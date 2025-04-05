namespace Prelude.Data.User

open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Mods
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Data.Library

[<RequireQualifiedAccess>]
type ScorePlayedBy =
    | You
    | Username of string

/// Everything you need to display a score screen or watch a replay of a score
type ScoreInfo =
    {
        ChartMeta: ChartMeta
        Chart: Chart
        WithMods: ModdedChart

        PlayedBy: ScorePlayedBy
        TimePlayed: int64
        Rate: Rate

        Replay: ReplayData
        mutable Scoring: ScoreProcessor
        mutable Lamp: int
        mutable Grade: int

        Rating: Difficulty
        Physical: float32

        ImportedFromOsu: bool
        IsFailed: bool
    }
    member this.Ruleset
        with get () = this.Scoring.Ruleset
        and set (ruleset) =
            let scoring =
                ScoreProcessor.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate

            this.Scoring <- scoring
            this.Lamp <- Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            this.Grade <- Grade.calculate ruleset.Grades scoring.Accuracy

    member this.WithRuleset (ruleset: Ruleset) =
        let scoring =
            ScoreProcessor.run ruleset this.WithMods.Keys (StoredReplayProvider this.Replay) this.WithMods.Notes this.Rate

        { this with
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            Grade = Grade.calculate ruleset.Grades scoring.Accuracy
        }

    member this.Accuracy = this.Scoring.Accuracy
    member this.Mods = this.WithMods.ModsApplied

    member this.ModStatus = this.WithMods.Status

    member this.ModString() =
        ModState.format (this.Rate, this.Mods)

module ScoreInfo =

    let from_score (chart_meta: ChartMeta) (chart: Chart) (ruleset: Ruleset) (score: Score) : ScoreInfo =
        let with_mods = ModState.apply score.Mods chart
        let replay_data = score.Replay |> Replay.decompress_bytes

        let scoring =
            ScoreProcessor.run ruleset with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.Rate

        let difficulty = Difficulty.calculate(score.Rate, with_mods.Notes)

        {
            ChartMeta = chart_meta
            Chart = chart
            WithMods = with_mods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = score.Timestamp
            Rate = score.Rate

            Replay = replay_data
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            Grade = Grade.calculate ruleset.Grades scoring.Accuracy

            Rating = difficulty
            Physical = Performance.calculate difficulty scoring

            ImportedFromOsu = score.IsImported
            IsFailed = score.IsFailed
        }

    let to_score (score_info: ScoreInfo) : Score =
        {
            Timestamp = score_info.TimePlayed
            Replay = score_info.Replay |> Replay.compress_bytes
            Rate = score_info.Rate
            Mods = score_info.Mods
            IsImported = score_info.ImportedFromOsu
            IsFailed = score_info.IsFailed
            Keys = score_info.WithMods.Keys
        }