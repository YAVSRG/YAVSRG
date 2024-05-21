namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Interlude.Content
open Interlude.Web.Shared
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options
open Interlude.Features.Play
open Interlude.Features.Online
open Interlude.Features.LevelSelect
open Interlude.Features
open Interlude.Features.Gameplay

type LobbySettingsPage(lobby: Lobby) =
    inherit Page()

    let settings = lobby.Settings

    let name = Setting.simple settings.Name
    let host_rotation = Setting.simple settings.HostRotation
    let auto_countdown = Setting.simple settings.AutomaticRoundCountdown

    override this.Content() =
        page_container()
        |+ PageTextEntry("lobby.name", name).Pos(0)
        |+ PageSetting("lobby.host_rotation", Checkbox host_rotation)
            .Tooltip(Tooltip.Info("lobby.host_rotation"))
            .Pos(3)
        |+ PageSetting("lobby.auto_countdown", Checkbox auto_countdown)
            .Tooltip(Tooltip.Info("lobby.auto_countdown"))
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
        |+ Conditional(
            (fun () -> lobby.YouAreHost),
            Button(Icons.SETTINGS, (fun () -> LobbySettingsPage(lobby).Show())),
            Position = Position.SliceTop(90.0f).Margin(10.0f).SliceRight(70.0f)
        )
        |+ Text(
            (fun () -> lobby.Settings.Name),
            Align = Alignment.CENTER,
            Position =
                { Position.SliceTop(90.0f).Margin(10.0f) with
                    Right = 0.4f %- 0.0f
                }
        )
        |+ PlayerList(lobby,
            Position =
                {
                    Left = 0.0f %+ 50.0f
                    Right = 0.4f %- 50.0f
                    Top = 0.0f %+ 100.0f
                    Bottom = 1.0f %- 100.0f
                }
        )
        |+ StylishButton(
            (fun () -> SelectedChart.if_loaded <| fun info -> Preview(info, ignore).Show()),
            K(sprintf "%s %s" Icons.EYE (%"levelselect.preview")),
            !%Palette.MAIN_100,
            TiltLeft = false,
            Hotkey = "preview",
            Position =
                { Position.SliceBottom(50.0f) with
                    Right = (0.4f / 3f) %- 25.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.preview"))
        |+ StylishButton(
            ignore,
            K(sprintf "%s %s" Icons.ZAP (%"levelselect.mods")),
            !%Palette.DARK_100,
            Hotkey = "mods",
            Position =
                { Position.SliceBottom(50.0f) with
                    Left = (0.4f / 3f) %- 0.0f
                    Right = (0.4f / 1.5f) %- 25.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.mods"))
        |+ Rulesets
            .QuickSwitcher(
                options.SelectedRuleset,
                Position =
                    {
                        Left = (0.4f / 1.5f) %+ 0.0f
                        Top = 1.0f %- 50.0f
                        Right = 0.4f %- 0.0f
                        Bottom = 1.0f %- 0.0f
                    }
            )
            .Tooltip(
                Tooltip
                    .Info("levelselect.rulesets", "ruleset_switch")
            )
        |+ SelectedChart(lobby,
            Position =
                {
                    Left = 0.5f %+ 20.0f
                    Top = 0.0f %+ 100.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.5f %- 0.0f
                }
        )
        |* Chat(lobby,
            Position =
                { Position.Margin(20.0f) with
                    Left = 0.4f %+ 20.0f
                    Top = 0.5f %+ 0.0f
                }
        )

        base.Init parent

        lobby.OnGameStart.Add(fun () ->
            if
                Screen.current_type = Screen.Type.Lobby
                && lobby.ReadyStatus = ReadyFlag.Play
            then
                SelectedChart.if_loaded
                <| fun info ->
                    if
                        Screen.change_new
                            (fun () -> PlayScreenMultiplayer.multiplayer_screen(info, lobby))
                            Screen.Type.Play
                            Transitions.Flags.Default
                        |> not
                    then
                        Logging.Warn("Missed the start of the lobby song because you were changing screen")

        )

        lobby.OnPlayerStatusChanged.Add(fun (username, status) ->
            if status = LobbyPlayerStatus.Playing then
                SelectedChart.if_loaded
                <| fun info ->

                let replay : OnlineReplayProvider = OnlineReplayProvider()
                let scoring = Metrics.create Rulesets.current info.WithMods.Keys replay info.WithMods.Notes SelectedChart.rate.Value
                let replay_info =
                    {
                        Replay = replay
                        ScoreMetric = scoring
                        GetScoreInfo = fun () ->
                            if not (replay :> IReplayProvider).Finished then
                                replay.Finish()
                            scoring.Update Time.infinity

                            let replay_data = (replay :> IReplayProvider).GetFullReplay()

                            {
                                CachedChart = info.CacheInfo
                                Chart = info.Chart
                                WithMods = info.WithMods

                                PlayedBy = ScorePlayedBy.Username username
                                TimePlayed = Timestamp.now ()
                                Rate = SelectedChart.rate.Value

                                Replay = replay_data
                                Scoring = scoring
                                Lamp = Lamp.calculate scoring.Ruleset.Grading.Lamps scoring.State
                                Grade = Grade.calculate scoring.Ruleset.Grading.Grades scoring.State

                                Rating = info.Rating
                                Patterns = info.Patterns
                                Physical = Performance.calculate info.Rating info.WithMods.Keys scoring |> fst

                                ImportedFromOsu = false
                            }
                    }
                lobby.AddReplayInfo(username, replay_info)

                if Screen.current_type = Screen.Type.Lobby && lobby.ReadyStatus = ReadyFlag.Spectate
                then
                    if
                        Screen.change_new
                            (fun () -> SpectateScreen.spectate_screen (info, username, replay_info, lobby))
                            Screen.Type.Replay
                            Transitions.Flags.Default
                        |> not
                    then
                        Logging.Warn("Missed the start of spectating because you were changing screen")
        )

// Screen

type LobbyScreen() =
    inherit Screen()

    // todo: rename ui when lobby changes
    let swap = SwapContainer()
    let current_lobby = None

    do
        NetworkEvents.join_lobby.Add(fun lobby -> 
            let lobby_ui = LobbyUI(lobby)
            swap.Current <- lobby_ui
        )

    override this.OnEnter(_) =
        match Network.lobby with
        | Some lobby -> LobbyChart.on_screen_enter lobby
        | None -> ()

        Song.on_finish <- SongFinishAction.LoopFromPreview
        DiscordRPC.in_menus ("Multiplayer lobby")

    override this.OnExit(_) = ()

    override this.OnBack() =
        match Network.lobby with
        | Some lobby ->
            ConfirmPage("Leave this lobby?", lobby.Leave).Show()
            None
        | None -> Some Screen.Type.LevelSelect

    override this.Init(parent) =
        this |* swap

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        match Network.lobby with
        | None -> if not Transitions.active then Screen.back Transitions.Flags.Default |> ignore
        | Some _ -> base.Update(elapsed_ms, moved)
