namespace Interlude.Web.Server.Domain.Services

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Gameplay.Mods
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Scores =

    [<RequireQualifiedAccess>]
    type ScoreUploadOutcome =
        | Failed
        | Unranked
        | Ranked of int option

    let private new_leaderboard_position (score: Score) : int option =
        if not score.Ranked then None else

        let existing_lb = Score2.get_leaderboard score.ChartId

        let mutable already_has_score = false
        let mutable position = -1
        let mutable i = 0

        while i < existing_lb.Length do
            if score.Accuracy > existing_lb.[i].Accuracy then
                position <- i
                i <- existing_lb.Length
            elif existing_lb.[i].UserId = score.UserId then
                already_has_score <- true
                i <- existing_lb.Length
            i <- i + 1

        if position < Score2.LEADERBOARD_SIZE && not already_has_score then
            Some position
        else None

    let submit
        (
            user_id: int64,
            chart_id: string,
            replay_untrusted_string: string,
            rate: Rate,
            mods: ModState,
            timestamp: int64
        ) =
        async {

            if rate < 0.5f<rate> || rate > 2.0f<rate> then
                return ScoreUploadOutcome.Failed
            else

            match Mods.check mods with
            | Error message ->
                Logging.Error "Mod validation failed from user #%i: %s" user_id message
                return ScoreUploadOutcome.Failed
            | Ok ModStatus.Unstored -> return ScoreUploadOutcome.Failed
            | Ok mod_ranked_status ->

            match! Backbeat.Charts.fetch.RequestAsync(chart_id) with
            | None -> return ScoreUploadOutcome.Unranked
            | Some chart ->

            match Replay.decompress_string_untrusted (chart.LastNote - chart.FirstNote) replay_untrusted_string with
            | Error message ->
                Logging.Error "Replay decompression failed from user #%i: %s" user_id message
                return ScoreUploadOutcome.Failed
            | Ok replay ->

            let is_ranked = rate >= 1.0f<rate> && mod_ranked_status = ModStatus.Ranked

            let mod_chart = Mods.apply mods chart

            let ruleset = Backbeat.rulesets.[Score2.PRIMARY_RULESET]

            let scoring =
                ScoreProcessor.run ruleset chart.Keys (StoredReplayProvider replay) mod_chart.Notes rate

            let accuracy = scoring.Accuracy

            if accuracy >= 0.7 then
                let score: Score =
                    Score2.create (
                        user_id,
                        chart_id,
                        timestamp,
                        rate,
                        mods,
                        is_ranked,
                        accuracy,
                        Grade.calculate ruleset.Grades scoring.Accuracy,
                        Lamp.calculate ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
                    )

                match new_leaderboard_position score with
                | Some p ->
                    let replay_id =
                        (user_id, chart_id, timestamp, replay)
                        |> Replay2.create
                        |> Replay2.save_leaderboard

                    let score_id = Score2.save (score.WithReplay replay_id)
                    Logging.Debug "Saved score %i with replay %i" score_id replay_id
                    return ScoreUploadOutcome.Ranked (Some (p + 1))
                | None ->
                    Score2.save score |> Logging.Debug "Saved score %i"
                    return ScoreUploadOutcome.Ranked None

            else

                return ScoreUploadOutcome.Unranked
        }

    let get_leaderboard_details (chart_id: string) =
        let leaderboard_scores = Score2.get_leaderboard chart_id

        let users =
            leaderboard_scores
            |> Array.map (fun x -> x.UserId)
            |> User.by_ids
            |> Map.ofArray

        let replays =
            leaderboard_scores
            |> Array.choose (fun x -> x.ReplayId)
            |> Replay2.by_ids
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