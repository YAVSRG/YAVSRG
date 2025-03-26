namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Calculator
open Prelude.Data.User
open Interlude.Content
open Interlude.Web.Shared
open Interlude.UI
open Interlude.Options
open Interlude.Features.Play
open Interlude.Features.Play.Spectate
open Interlude.Features.Online
open Interlude.Features.LevelSelect
open Interlude.Features.Rulesets
open Interlude.Features.Gameplay

type LobbySettingsPage(lobby: Lobby) =
    inherit Page()

    let settings = lobby.Settings

    let name = Setting.simple settings.Name
    let host_rotation = Setting.simple settings.HostRotation
    let auto_countdown = Setting.simple settings.AutomaticRoundCountdown

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"lobby.name", name).Pos(0)
        |+ PageSetting(%"lobby.host_rotation", Checkbox host_rotation)
            .Help(Help.Info("lobby.host_rotation"))
            .Pos(3)
        |+ PageSetting(%"lobby.auto_countdown", Checkbox auto_countdown)
            .Help(Help.Info("lobby.auto_countdown"))
            .Pos(5)
        :> Widget

    override this.Title = %"lobby"

    override this.OnClose() =
        lobby.ChangeSettings
            {
                Name = name.Value
                HostRotation = host_rotation.Value
                AutomaticRoundCountdown = auto_countdown.Value
            }

type LobbyUI(lobby: Lobby) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |+ Button(Icons.SETTINGS, fun () -> LobbySettingsPage(lobby).Show())
            .Position(Position.SliceT(90.0f).Shrink(10.0f).SliceR(70.0f))
            .Conditional(fun () -> lobby.YouAreHost)
        |+ Text(fun () -> lobby.Settings.Name)
            .Align(Alignment.CENTER)
            .Position(Position.SliceT(90.0f).ShrinkPercentL(0.4f).Shrink(10.0f))
        |+ PlayerList(lobby)
            .Position(Position.SlicePercentL(0.4f).Shrink(50.0f, 100.0f))
        |+ AngledButton(
            sprintf "%s %s" Icons.EYE (%"levelselect.preview"),
            (fun () -> SelectedChart.if_loaded <| fun info -> Preview(info, ignore).Show()),
            Palette.MAIN_100
        )
            .LeanLeft(false)
            .Hotkey("preview")
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .SlicePercentL(0.4f)
                    .GridX(1, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.preview"))
        |+ AngledButton(
            sprintf "%s %s" Icons.ZAP (%"levelselect.mods"),
            ignore,
            Palette.DARK_100
        )
            .Hotkey("mods")
            .Disabled()
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .SlicePercentL(0.4f)
                    .GridX(2, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.mods"))
        |+ RulesetSwitcher(options.SelectedRuleset)
            .Position(
                Position
                    .SliceB(AngledButton.HEIGHT)
                    .SlicePercentL(0.4f)
                    .GridX(3, 3, AngledButton.LEAN_AMOUNT)
            )
            .Help(Help.Info("levelselect.rulesets", "ruleset_switch"))
        |+ SelectedChart(lobby)
            .Position(Position.SlicePercentT(0.5f).SlicePercentR(0.5f).ShrinkX(20.0f).ShrinkT(100.0f))
        |* Chat(lobby)
            .Position(
                Position
                    .ShrinkPercentL(0.4f)
                    .SlicePercentB(0.5f)
                    .ShrinkX(20.0f)
                    .ShrinkB(20.0f)
            )

        base.Init parent

        lobby.OnGameStart.Add(fun () ->
            if
                Screen.current_type = ScreenType.Lobby
                && lobby.ReadyStatus = ReadyFlag.Play
            then
                SelectedChart.if_loaded
                <| fun info ->
                    if
                        Screen.change_new
                            (fun () -> PlayScreenMultiplayer.multiplayer_screen(info, lobby))
                            ScreenType.Play
                            Transitions.EnterGameplayFadeAudio
                        |> not
                    then
                        Logging.Warn("Missed the start of the lobby song because you were changing screen")
        )

        lobby.OnPlayerStatusChanged.Add(fun (username, status) ->
            if status = LobbyPlayerStatus.Playing then
                SelectedChart.if_loaded
                <| fun info ->

                let replay : OnlineReplayProvider = OnlineReplayProvider()
                let scoring = ScoreProcessor.create Rulesets.current info.WithMods.Keys replay info.WithMods.Notes SelectedChart.rate.Value
                let replay_info =
                    {
                        Replay = replay
                        ScoreProcessor = scoring
                        GetScoreInfo = fun () ->
                            if not (replay :> IReplayProvider).Finished then
                                replay.Finish()
                            scoring.Update Time.infinity

                            let replay_data = (replay :> IReplayProvider).GetFullReplay()

                            {
                                ChartMeta = info.ChartMeta
                                Chart = info.Chart
                                WithMods = info.WithMods

                                PlayedBy = ScorePlayedBy.Username username
                                TimePlayed = Timestamp.now ()
                                Rate = SelectedChart.rate.Value

                                Replay = replay_data
                                Scoring = scoring
                                Lamp = Lamp.calculate scoring.Ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
                                Grade = Grade.calculate scoring.Ruleset.Grades scoring.Accuracy

                                Rating = info.Difficulty
                                Physical = Performance.calculate info.Difficulty scoring

                                ImportedFromOsu = false
                                IsFailed =
                                    match lobby.Players.TryGetValue username with
                                    | true, player -> player.Status = LobbyPlayerStatus.AbandonedPlay
                                    | false, _ -> true
                            }
                    }
                lobby.AddReplayInfo(username, replay_info)

                if Screen.current_type = ScreenType.Lobby && lobby.ReadyStatus = ReadyFlag.Spectate
                then
                    if
                        Screen.change_new
                            (fun () -> Spectate.spectate_screen (info, username, replay_info, lobby))
                            ScreenType.Replay
                            Transitions.Default
                        |> not
                    then
                        Logging.Warn("Missed the start of spectating because you were changing screen")
        )

// Screen

type LobbyScreen() =
    inherit Screen()

    let current_lobby_ui = SwapContainer()
    let current_lobby = None

    do
        NetworkEvents.join_lobby.Add(fun lobby ->
            let lobby_ui = LobbyUI(lobby)
            current_lobby_ui.Current <- lobby_ui
        )

    override this.OnEnter(_) =
        match Network.lobby with
        | Some lobby -> LobbyChart.on_screen_enter lobby
        | None -> ()

        Song.on_finish <- SongFinishAction.LoopFromPreview
        DiscordRPC.in_menus ("Multiplayer lobby")

    override this.OnExit(_) =
        Selection.clear()
        Input.remove_listener()

    override this.OnBack() =
        match Network.lobby with
        | Some lobby ->
            ConfirmPage("Leave this lobby?", lobby.Leave).Show()
            None
        | None -> Some ScreenType.LevelSelect

    override this.Init(parent) =
        this |* current_lobby_ui

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        match Network.lobby with
        | None -> if not (Transitions.in_progress()) then Screen.back Transitions.Default |> ignore
        | Some _ -> base.Update(elapsed_ms, moved)