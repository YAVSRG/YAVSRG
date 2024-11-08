namespace Interlude.Features.Play.HUD

open System.Collections.Generic
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude
open Interlude.Features.Online
open Interlude.Features.Play

// todo: config on your username color vs other peoples
type MultiplayerScoreTracker(config: HudConfig, state: PlayState, replays: Dictionary<string, LobbyPlayerReplayInfo>) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let x = this.Bounds.CenterX
        let mutable y = this.Bounds.Top

        replays
        |> Seq.map (|KeyValue|)
        |> Seq.sortByDescending (fun (_: string, replay_info: LobbyPlayerReplayInfo) -> replay_info.ScoreProcessor.Accuracy)
        |> Seq.iter (fun (username: string, replay_info: LobbyPlayerReplayInfo) ->
            let color =
                if username = Network.credentials.Username then
                    Color.SkyBlue
                else
                    Color.White

            Text.draw (Style.font, username, 20.0f, x, y, color)
            Text.draw_aligned (Style.font, replay_info.ScoreProcessor.FormattedAccuracy, 20.0f, x - 10.0f, y, color, 1.0f)
            y <- y + 25.0f
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for replay_info in replays.Values do
            replay_info.ScoreProcessor.Update(
                state.CurrentChartTime()
                - Web.Shared.Packets.MULTIPLAYER_REPLAY_DELAY_MS * 2.0f<ms>
            )