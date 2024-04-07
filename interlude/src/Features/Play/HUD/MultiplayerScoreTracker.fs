namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Content.Noteskins
open Interlude
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Gameplay

// todo: give this thing its own placement config + config on your username color vs other peoples
type MultiplayerScoreTracker(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let x = this.Bounds.Right + 100.0f
        let mutable y = this.Bounds.Top

        Multiplayer.replays
        |> Seq.map (|KeyValue|)
        |> Seq.sortByDescending (fun (_, (s, _)) -> s.Value)
        |> Seq.iter (fun (username, (s, _)) ->
            let c =
                if username = Network.credentials.Username then
                    Color.SkyBlue
                else
                    Color.White

            Text.draw (Style.font, username, 20.0f, x, y, c)
            Text.draw_aligned (Style.font, s.FormatAccuracy(), 20.0f, x - 10.0f, y, c, 1.0f)
            y <- y + 25.0f
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for s, _ in Multiplayer.replays.Values do
            s.Update(
                state.CurrentChartTime()
                - Web.Shared.Packets.MULTIPLAYER_REPLAY_DELAY_MS * 2.0f<ms>
            )