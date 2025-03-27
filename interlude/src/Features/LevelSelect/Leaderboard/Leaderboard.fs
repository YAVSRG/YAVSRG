namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay

module ScoreSync =

    let mutable private lb = false
    let mutable private local = false
    let mutable private synced = false

    let sync_if_ready() =
        if not synced && lb && local then

            let best_eligible_score =
                match SelectedChart.SAVE_DATA with
                | Some data ->
                    match data.PersonalBests |> Bests.ruleset_best_above SC_J4_HASH (_.Accuracy) 1.0f<rate> with
                    | Some (acc, _, timestamp) ->
                        match LocalScores.local_scores |> Seq.tryFind (fun s -> s.TimePlayed = timestamp) with
                        | Some local_score when not local_score.ImportedFromOsu ->
                            Some (local_score, timestamp)
                        | _ -> None
                    | None -> None
                | None -> None

            match best_eligible_score with
            | None -> ()
            | Some (local_score, timestamp) ->
                let leaderboard_has_room =
                    OnlineScores.leaderboard_scores
                    |> Seq.exists (fun s -> s.TimePlayed = timestamp)
                    |> not
                    && OnlineScores.leaderboard_scores.Count < 20

                if leaderboard_has_room then
                    Gameplay.upload_score local_score

            synced <- true

    let init() =
        SelectedChart.on_chart_change_started.Add(fun _ -> lb <- false; local <- false; synced <- false)
        OnlineScores.leaderboard_loaded.Add(fun b -> lb <- b; sync_if_ready())
        LocalScores.scores_loaded.Add(fun _ -> local <- true; sync_if_ready())

type Leaderboard(display: Setting<InfoPanelMode>) =
    inherit Container(NodeType.None)

    let mutable count = 0
    let mutable load_if_visible = true

    let filter : Setting<Filter> = Setting.simple Filter.None
    let sort : Setting<Sort> = Setting.map enum int options.ScoreSortMode

    let scores_list = FlowContainer.Vertical<LeaderboardCard>(75.0f, Spacing = Style.PADDING * 3.0f)

    do
        ScoreSync.init()
        OnlineScores.leaderboard_score_loaded.Add (fun score_info -> score_info |> LeaderboardCard |> scores_list.Add; count <- count + 1)

    override this.Init(parent) =
        SelectedChart.on_chart_change_started.Add (fun _ -> scores_list.Iter(fun s -> s.FadeOut()))
        SelectedChart.on_chart_change_finished.Add (fun _ -> scores_list.Clear(); count <- 0; load_if_visible <- true)
        Gameplay.leaderboard_rank_changed.Add (fun _ -> scores_list.Clear(); count <- 0)

        this
        |+ AngledButton(
            %"levelselect.info.leaderboard",
            (fun () -> display.Set InfoPanelMode.Patterns),
            Palette.MAIN_100
        )
            .Hotkey("scoreboard_storage")
            .LeanLeft(false)
            .Position(
                Position
                    .SliceT(AngledButton.HEIGHT)
                    .GridX(1, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.info.mode", "scoreboard_storage"))
        |+ AngledButton(
            Icons.CHEVRONS_UP + " " + %"levelselect.info.scoreboard.sort.accuracy",
            ignore,
            Palette.DARK_100
        )
            .Hotkey("scoreboard_sort")
            .Disabled()
            .Position(
                Position
                    .SliceT(AngledButton.HEIGHT)
                    .GridX(2, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.info.scoreboard.sort", "scoreboard_sort"))
        |+ AngledButton(
            Icons.FILTER + " " + %"levelselect.info.scoreboard.filter.none",
            ignore,
            Palette.MAIN_100
        )
            .Hotkey("scoreboard_filter")
            .Disabled()
            .LeanRight(false)
            .Position(
                Position
                    .SliceT(AngledButton.HEIGHT)
                    .GridX(3, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.info.scoreboard.filter", "scoreboard_filter"))
        |+ ScrollContainer(scores_list)
            .Margin(Style.PADDING)
            .Position(Position.ShrinkT(50.0f))
        |+ HotkeyListener("scoreboard", fun () ->
            if scores_list.Focused then
                Selection.clear ()
            else
                scores_list.Focus false
        )
        |+ EmptyState(
            Icons.FLAG,
            %"levelselect.info.leaderboard.empty",
            Subtitle = %"levelselect.info.leaderboard.empty.subtitle"
        )
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.Loaded && count = 0)
        |+ EmptyState(Icons.CLOUD_OFF, %"levelselect.info.leaderboard.unavailable", Subtitle = %"levelselect.info.leaderboard.unavailable.subtitle")
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.NoLeaderboard)
        |* EmptyState(Icons.GLOBE, %"misc.offline")
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.Offline)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        if load_if_visible then
            SelectedChart.if_loaded OnlineScores.load
            load_if_visible <- false
        base.Update(elapsed_ms, moved)
        OnlineScores.score_loader.Join()