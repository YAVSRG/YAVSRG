namespace Interlude.Features.Play.Spectate

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Gameplay.Replays
open Interlude.Web.Shared.Packets
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play

type SpectateScreen =

    static member Create(info: LoadedChartInfo, username: string, replay_info: LobbyPlayerReplayInfo, lobby: Lobby) : Screen =

        let mutable currently_spectating = username
        let mutable scoring = replay_info.ScoreProcessor
        let mutable replay_data : OnlineReplay = replay_info.Replay :?> OnlineReplay

        let cycle_spectator (screen: IPlayScreen) =
            let users_available_to_spectate =
                lobby.Replays.Keys
                |> Seq.where (fun name -> name <> Network.credentials.Username)
                |> Array.ofSeq

            let next_user =
                match Array.tryFindIndex (fun u -> u = currently_spectating) users_available_to_spectate with
                | None -> users_available_to_spectate.[0]
                | Some i -> users_available_to_spectate.[(i + 1) % users_available_to_spectate.Length]

            match lobby.GetReplayInfo next_user with
            | Some replay_info ->
                currently_spectating <- next_user
                scoring <- replay_info.ScoreProcessor
                replay_data <- replay_info.Replay :?> OnlineReplay
                Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                screen.State.ChangeScoring scoring
            | None -> Logging.Warn "Failed to switch to replay data for %s" next_user

        let mutable wait_for_load = 1000.0
        let mutable exiting = false

        lobby.StartSpectating()

        { new IPlayScreen(info, PacemakerState.None, scoring, HudContextInner.Spectate lobby.Replays) with
            override this.Init(parent: Widget) =

                this
                    .Add(
                        SpectateOverlay(
                            info,
                            ignore,
                            (fun () -> currently_spectating),
                            fun () -> cycle_spectator this
                        )
                )

                base.Init(parent)

            override this.OnEnter(prev) =
                base.OnEnter(prev)
                DiscordRPC.playing ("Spectating", info.ChartMeta.Title)
                Song.pause ()

            override this.OnExit(next) =
                base.OnExit(next)
                Song.resume ()
                Toolbar.show_cursor ()

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)

                if wait_for_load > 0.0 then
                    wait_for_load <- wait_for_load - elapsed_ms

                    if wait_for_load <= 0 then
                        Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                        Song.resume ()
                else

                let chart_time = this.State.CurrentChartTime()

                if replay_data.Time() - chart_time < MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms> then
                    if Song.playing () then
                        Song.pause ()
                elif not (Song.playing ()) then
                    Song.resume ()

                scoring.Update chart_time

                if this.State.Scoring.Finished && not exiting then
                    exiting <- true
                    Screen.back Transitions.Default |> ignore
        }