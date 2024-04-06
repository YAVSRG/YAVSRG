namespace Interlude.Web.Server.Domain.Services

open Percyqaz.Common
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Scores =

    type LeaderboardRankChange =
        {
            RulesetId: string
            OldRank: int64 option
            NewRank: int64
        }

    type TableRatingChange =
        {
            Table: string
            OldPosition: (int64 * float) option
            NewPosition: (int64 * float)
        }

    [<RequireQualifiedAccess>]
    type ScoreUploadOutcome =
        | UploadFailed
        | SongNotRecognised
        | Unrated
        | Rated of TableRatingChange list * LeaderboardRankChange list

    let private ACCEPTED_RULESETS = [| Score.PRIMARY_RULESET |]

    let submit
        (
            user_id: int64,
            chart_id: string,
            replay_untrusted_string: string,
            rate: float32,
            mods: ModState,
            timestamp: int64
        ) =
        async {

            if rate < 0.5f || rate > 2.0f then
                return ScoreUploadOutcome.UploadFailed
            else

            match Mods.check mods with
            | Error message ->
                Logging.Error(sprintf "Mod validation failed from user #%i: %s" user_id message)
                return ScoreUploadOutcome.UploadFailed
            | Ok ModStatus.Unstored -> return ScoreUploadOutcome.UploadFailed
            | Ok mod_ranked_status ->

            match! Backbeat.Charts.fetch.RequestAsync(chart_id) with
            | None -> return ScoreUploadOutcome.SongNotRecognised
            | Some chart ->

            match Replay.decompress_string_untrusted (chart.LastNote - chart.FirstNote) replay_untrusted_string with
            | Error message ->
                Logging.Error(sprintf "Replay decompression failed from user #%i: %s" user_id message)
                return ScoreUploadOutcome.UploadFailed
            | Ok replay ->

            let is_ranked = rate >= 1.0f && mod_ranked_status = ModStatus.Ranked

            let mod_chart = Mods.apply mods chart

            if not is_ranked then
                return ScoreUploadOutcome.Unrated
            else

            let mutable leaderboard_changes: LeaderboardRankChange list = []
            let mutable table_changes: TableRatingChange list = []

            // todo: get relevant rulesets for this chart (i.e. tables ft this chart with different rulesets + SCJ4)
            for ruleset_id in ACCEPTED_RULESETS do
                let ruleset = Backbeat.rulesets.[ruleset_id]

                let scoring =
                    Metrics.run ruleset chart.Keys (StoredReplayProvider replay) mod_chart.Notes rate

                let accuracy = scoring.Value

                if accuracy < 0.7 then
                    ()
                else

                    let score: Score =
                        Score.create (
                            user_id,
                            chart_id,
                            ruleset_id,
                            timestamp,
                            rate,
                            mods,
                            is_ranked,
                            accuracy,
                            Grade.calculate ruleset.Grading.Grades scoring.State,
                            Lamp.calculate ruleset.Grading.Lamps scoring.State
                        )

                    let is_new_leaderboard_score =
                        if is_ranked && Leaderboard.exists chart_id ruleset_id then
                            match Score.get_user_leaderboard_score user_id chart_id ruleset_id with
                            | Some score_model -> score_model.Accuracy < accuracy
                            | None -> true
                        else
                            false

                    let score =
                        if is_new_leaderboard_score then
                            let replay_id =
                                Replay.create (user_id, chart_id, timestamp, replay)
                                |> Replay.save_leaderboard ruleset_id

                            score.WithReplay replay_id
                        else
                            score

                    Score.save score |> ignore

            Tables.recalculate_affected_table_ratings (user_id, chart_id) // todo: return what ratings changed

            return ScoreUploadOutcome.Rated(table_changes, leaderboard_changes)
        }

    let get_leaderboard_details (chart_id: string) (ruleset_id: string) =
        let leaderboard_scores = Score.get_leaderboard chart_id ruleset_id

        let users =
            leaderboard_scores
            |> Array.map (fun x -> x.UserId)
            |> User.by_ids
            |> Map.ofArray

        let replays =
            leaderboard_scores
            |> Array.choose (fun x -> x.ReplayId)
            |> Replay.by_ids
            |> Map.ofArray

        leaderboard_scores
        |> Array.indexed
        |> Array.choose (fun (i, score) ->
            match users.TryFind score.UserId with
            | None -> None
            | Some user ->

            match score.ReplayId with
            | None -> None
            | Some replay_id ->

            match replays.TryFind replay_id with
            | None -> None
            | Some replay ->

            Some(i, user, score, replay)
        )
