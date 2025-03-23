namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Prelude.Charts
open Prelude.Calculator
open Prelude.Mods
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

module OnlineScores =

    [<RequireQualifiedAccess>]
    type State =
        | Offline = -2
        | NoLeaderboard = -1
        | Unloaded = 0
        | Loading = 1
        | Loaded = 2

    type Request =
        {
            Ruleset: Ruleset
            ChartMeta: ChartMeta
            CurrentChart: Chart
        }
        override this.ToString() = "<leaderboard loader>"

    // Can be accessed from the UI thread at any time
    // Contains scores for the current loaded leaderboard, calculated for current ruleset
    let leaderboard_scores : ResizeArray<ScoreInfo> = ResizeArray()
    let mutable state = State.Unloaded

    let private leaderboard_score_loaded_ev = Event<LeaderboardScore * ScoreInfo>()
    let leaderboard_score_loaded = leaderboard_score_loaded_ev.Publish

    let private leaderboard_loaded_ev = Event<bool>()
    let leaderboard_loaded = leaderboard_loaded_ev.Publish

    let private load_score (score: LeaderboardScore, score_info: ScoreInfo) : unit =
        if score_info.Ruleset <> Rulesets.current then
            score_info.Ruleset <- Rulesets.current
        leaderboard_scores.Add score_info
        leaderboard_score_loaded_ev.Trigger (score, score_info)

    let private finish_loading (new_state: State) : unit =
        state <- new_state
        leaderboard_loaded_ev.Trigger (state <> State.NoLeaderboard)

    let score_loader =
        { new Async.CancelQueueSeq<Request, unit -> unit>() with
            member this.Process(req: Request) =
                seq {
                    let mutable scores : LeaderboardScore array = [||]
                    let mutable new_state = State.Offline
                    Charts.Scores.Leaderboard.get_async (
                        req.ChartMeta.Hash,
                        function
                        | Some reply ->
                            new_state <- State.Loaded
                            scores <- reply.Scores
                        | None ->
                            new_state <- State.NoLeaderboard
                    )
                    |> Async.RunSynchronously

                    for score in scores do
                        let with_mods = ModState.apply score.Mods req.CurrentChart
                        let replay_data = Replay.decompress_string score.Replay

                        let scoring =
                            ScoreProcessor.run
                                req.Ruleset
                                with_mods.Keys
                                (StoredReplayProvider replay_data)
                                with_mods.Notes
                                score.Rate

                        let rating = Difficulty.calculate(score.Rate, with_mods.Notes)

                        let score_info: ScoreInfo =
                            {
                                ChartMeta = req.ChartMeta
                                Chart = req.CurrentChart
                                WithMods = with_mods

                                PlayedBy = ScorePlayedBy.Username score.Username
                                TimePlayed = score.Timestamp
                                Rate = score.Rate

                                Replay = replay_data
                                Scoring = scoring
                                Lamp = Lamp.calculate req.Ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
                                Grade = Grade.calculate req.Ruleset.Grades scoring.Accuracy

                                Rating = rating
                                Physical = Performance.calculate rating scoring

                                ImportedFromOsu = false
                                IsFailed = false
                            }
                        yield fun () -> load_score (score, score_info)
                    yield fun () -> finish_loading new_state
                }

            member this.Handle(action) = action ()
        }

    let load (info: LoadedChartInfo) : unit =
        if Network.status <> Network.Status.LoggedIn then
            state <- State.Offline
        elif state = State.Unloaded then
            score_loader.Request
                {
                    Ruleset = Rulesets.current
                    ChartMeta = info.ChartMeta
                    CurrentChart = info.Chart
                }

    do
        SelectedChart.on_chart_change_started.Add (fun info ->
            leaderboard_scores.Clear()
            state <- State.Unloaded
            score_loader.Cancel()
        )

        Rulesets.on_changed.Add (fun ruleset ->
            for score_info in leaderboard_scores do
                score_info.Ruleset <- ruleset
        )

        Gameplay.leaderboard_rank_changed.Add (fun info ->
            if state = State.Loaded then
                score_loader.Request
                    {
                        Ruleset = Rulesets.current
                        ChartMeta = info.ChartMeta
                        CurrentChart = info.Chart
                    }
        )