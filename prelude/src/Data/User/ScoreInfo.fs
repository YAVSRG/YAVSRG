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

        Replay: Replay
        mutable Scoring: ScoreProcessor
        mutable Lamp: int
        mutable Grade: int

        Rating: Difficulty
        Performance: float32

        ImportedFromOsu: bool
        IsFailed: bool
    }
    member this.Ruleset
        with get () = this.Scoring.Ruleset
        and set ruleset =
            let scoring = ScoreProcessor.ProcessEntireReplay(ruleset, this.Replay, this.WithMods, this.Rate)
            this.Scoring <- scoring
            this.Lamp <- Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            this.Grade <- Grade.calculate ruleset.Grades scoring.Accuracy

    member this.WithRuleset(ruleset: Ruleset) : ScoreInfo =
        let scoring = ScoreProcessor.ProcessEntireReplay(ruleset, this.Replay, this.WithMods, this.Rate)

        { this with
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            Grade = Grade.calculate ruleset.Grades scoring.Accuracy
        }

    member this.Accuracy = this.Scoring.Accuracy
    member this.Mods : ModState = this.WithMods.ModsApplied

    member this.ModStatus : ModStatus = this.WithMods.Status

    member this.ModString() : string = ModState.format (this.Rate, this.Mods)
    member this.Shorthand : string = sprintf "%s | %s" this.Scoring.FormattedAccuracy (this.Ruleset.LampName this.Lamp)
    
    member this.ToScore() : Score =
        {
            Timestamp = this.TimePlayed
            Replay = this.Replay.ToByteArray()
            Rate = this.Rate
            Mods = this.Mods
            IsImported = this.ImportedFromOsu
            IsFailed = this.IsFailed
            Keys = this.WithMods.Keys
        }
        
    static member CreateFromScore(chart_meta: ChartMeta, chart: Chart, ruleset: Ruleset, score: Score) : ScoreInfo =
        let with_mods = ModState.apply score.Mods chart
        let replay = Replay.FromByteArray(score.Replay)
        let scoring = ScoreProcessor.ProcessEntireReplay(ruleset, replay, with_mods, score.Rate)
        let difficulty = Difficulty.calculate(score.Rate, with_mods.ToNoteData())

        {
            ChartMeta = chart_meta
            Chart = chart
            WithMods = with_mods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = score.Timestamp
            Rate = score.Rate

            Replay = replay
            Scoring = scoring
            Lamp = Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            Grade = Grade.calculate ruleset.Grades scoring.Accuracy

            Rating = difficulty
            Performance = Performance.calculate difficulty scoring

            ImportedFromOsu = score.IsImported
            IsFailed = score.IsFailed
        }