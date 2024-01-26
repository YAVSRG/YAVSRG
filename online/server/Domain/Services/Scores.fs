namespace Interlude.Web.Server.Domain.Services

open Prelude
open Prelude.Charts
open Prelude.Gameplay
open Interlude.Web.Server.Domain.Objects

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
        | SongNotRecognised
        | Unrated
        | Rated of TableRatingChange list * LeaderboardRankChange list

    let private ACCEPTED_RULESETS = [| Score.PRIMARY_RULESET |]
    
    let submit (user_id: int64, chart_id: string, replay: ReplayData, rate: float32, mods: Mods.ModState, timestamp: int64) =
        async {

            match! Interlude.Web.Server.Domain.Backbeat.Charts.fetch.RequestAsync(chart_id) with
            | None -> return ScoreUploadOutcome.SongNotRecognised
            | Some chart ->

            let mod_chart = Mods.apply_mods mods chart

            let is_ranked = rate >= 1.0f && (match Mods.check mods with Ok Mods.ModStatus.Ranked -> true | _ -> false)

            if not is_ranked then
                return ScoreUploadOutcome.Unrated
            else

            let mutable leaderboard_changes: LeaderboardRankChange list = []
            let mutable table_changes: TableRatingChange list = []

            for ruleset_id in ACCEPTED_RULESETS do
                let ruleset = Interlude.Web.Server.Domain.Backbeat.rulesets.[ruleset_id]
                let scoring = Metrics.create ruleset chart.Keys (StoredReplayProvider replay) mod_chart.Notes rate

                scoring.Update Time.infinity

                let accuracy = scoring.Value

                let score : Score =
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
                    else false

                let score =
                    if is_new_leaderboard_score then
                        let replay_id = 
                            Replay.create (user_id, chart_id, timestamp, replay)
                            |> Replay.save_leaderboard ruleset_id
                        score.WithReplay replay_id
                    else score

                Score.save score |> ignore

                // todo: table and leaderboard changes

            return ScoreUploadOutcome.Rated (table_changes, leaderboard_changes)
        }

    let get_leaderboard_details (chart_id: string) (ruleset_id: string) =
        let leaderboard_scores = Score.get_leaderboard chart_id ruleset_id
        let users = leaderboard_scores |> Array.map (fun x -> x.UserId) |> User.by_ids |> Map.ofArray
        let replays = leaderboard_scores |> Array.choose (fun x -> x.ReplayId) |> Replay.by_ids |> Map.ofArray

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

            Some (i, user, score, replay)
        )