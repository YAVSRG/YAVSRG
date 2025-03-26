namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.Web.Shared
open Interlude.UI
open Interlude.Features.Score
open Interlude.Features.Online

type Chat(lobby: Lobby) =
    inherit Container(NodeType.None)

    let MESSAGE_HEIGHT = 40.0f

    let current_message = Setting.simple ""

    let chat_msg (sender: string, message: string) : Container =
        let w = Text.measure (Style.font, sender) * 0.6f * MESSAGE_HEIGHT

        let sender_color =
            if sender = Network.credentials.Username then
                Colors.text_subheading // todo: know your own username color
            else
                lobby.Players.[sender].Color, Colors.shadow_2

        Container(NodeType.None)
        |+ Text(sender)
            .Color(sender_color)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceL(w))
        |+ Text(": " + message)
            .Color(Colors.text)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkL(w))

    let messages = FlowContainer.Vertical<Widget>(MESSAGE_HEIGHT, Spacing = 2.0f)

    let message_history =
        ScrollContainer(messages)
            .Position(Position.ShrinkB(60.0f).Shrink(5.0f))

    let chatline =
        TextEntry(current_message, "none", true)
            .Position(Position.SliceB(50.0f).Shrink(5.0f))

    let mutable last_msg: Widget option = None

    let add_msg (w: Widget) =
        messages.Add w

        match last_msg with
        | Some m ->
            if m.Bounds.Top - message_history.RemainingScrollAnimation - message_history.Bounds.Bottom < 200.0f then
                GameThread.defer (fun () -> message_history.Scroll infinityf)
        | None -> ()

        last_msg <- Some w

    let game_end_report () =
        let replays = lobby.Replays
        if replays.Keys.Count > 0 then
            add_msg (
                Text(sprintf "== Results for: %s ==" lobby.Chart.Value.Title)
                    .Color(Colors.text)
                    .Align(Alignment.CENTER)
            )

            let scores =
                replays.Keys
                |> Seq.map (fun username ->
                    let replay_info = replays.[username]
                    replay_info.ScoreProcessor.Update Time.infinity
                    username, replay_info.GetScoreInfo()
                )
                |> Seq.sortByDescending (fun (_, s) -> s.Scoring.Accuracy)

            let mutable place = 0

            for username, data in scores do
                place <- place + 1

                let color =
                    match place with
                    | 1 -> Color.Gold
                    | 2 -> Color.Silver
                    | 3 -> Color.DarkOrange
                    | _ -> Color.White

                let cmp =
                    let lamp = data.Lamp
                    let grade = data.Grade

                    let container =
                        { new Container(NodeType.Leaf) with
                            override this.Draw() =
                                if this.Focused then
                                    Render.rect this.Bounds Colors.grey_2.O1

                                base.Draw()
                        }

                    container
                    |+ MouseListener()
                        .OnLeftClick(fun () ->
                            Screen.change_new
                                (fun () -> ScoreScreen(data, (ImprovementFlags.None, None), false) :> Screen)
                                ScreenType.Score
                                Transitions.EnterGameplayNoFadeAudio
                            |> ignore
                        )
                        .FocusOnHover(container)
                    |+ Text(sprintf "%i. %s" place username)
                        .Color(fun () ->
                            if container.Focused then
                                Colors.text_yellow_2
                            else
                                (color, Colors.shadow_1)
                        )
                        .Align(Alignment.LEFT)
                    |+ Text(data.Scoring.FormattedAccuracy)
                        .Color(data.Ruleset.GradeColor grade, Colors.shadow_1)
                        .Align(Alignment.CENTER)
                    |+ Text(data.Ruleset.LampName lamp)
                        .Color(data.Ruleset.LampColor lamp, Colors.shadow_1)
                        .Align(0.75f)
                    |+ Text(sprintf "%ix" (data.Scoring.BestCombo))
                        .Color(data.Ruleset.LampColor lamp, Colors.shadow_1)
                        .Align(Alignment.RIGHT)

                add_msg cmp

            add_msg (
                Text("Click a score to view details")
                    .Color(Colors.text_greyout)
                    .Align(Alignment.CENTER)
            )

    let countdown (reason, seconds) =
        let now = System.DateTime.UtcNow

        let seconds_left () =
            let elapsed =
                (System.DateTime.UtcNow - now).TotalSeconds |> System.Math.Floor |> int

            max 0 (seconds - elapsed)

        Container(NodeType.None)
        |+ Text(fun () -> sprintf "%s %s: %i" Icons.CLOCK reason (seconds_left ()))
            .Color(fun () ->
                if seconds_left () > 0 then
                    Colors.text_green_2
                else
                    Colors.text_greyout
            )
            .Align(Alignment.CENTER)
        |> add_msg

    override this.Init(parent) =
        this
        |+ chatline
        |+ Text("Press ENTER to chat")
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.SliceB(50.0f).Shrink(5.0f))
            .Conditional(fun () -> current_message.Value = "")
        |* message_history

        lobby.OnChatMessage.Add(chat_msg >> add_msg)
        lobby.OnSystemMessage.Add(fun msg -> add_msg (Text(msg).Align(Alignment.CENTER)))

        lobby.OnLobbyEvent.Add(fun (kind, data) ->
            let text, color =
                match (kind, data) with
                | LobbyEvent.Join, who -> sprintf "%s %s joined" Icons.LOG_IN who, Colors.green_accent
                | LobbyEvent.Leave, who -> sprintf "%s %s left" Icons.LOG_OUT who, Colors.red_accent
                | LobbyEvent.Host, who -> sprintf "%s %s is now host" Icons.STAR who, Colors.yellow_accent
                | LobbyEvent.Ready, who -> sprintf "%s %s is ready" Icons.CHECK who, Colors.green
                | LobbyEvent.Ready_Spectate, who -> sprintf "%s %s is ready" Icons.EYE who, Colors.green
                | LobbyEvent.NotReady, who -> sprintf "%s %s is not ready" Icons.X who, Colors.pink
                | LobbyEvent.Invite, who -> sprintf "%s %s invited" Icons.MAIL who, Colors.cyan_accent
                | LobbyEvent.Generic, msg -> sprintf "%s %s" Icons.INFO msg, Colors.grey_1
                | _, msg -> msg, Colors.white

            add_msg(
                Text(text)
                    .Color((fun () -> color, Colors.shadow_1))
                    .Align(Alignment.CENTER)
            )
        )

        lobby.OnGameEnd.Add game_end_report
        lobby.OnCountdown.Add countdown

        base.Init parent

    override this.Draw() =
        Render.rect (this.Bounds.ShrinkB 60.0f) Colors.shadow_2.O3
        Render.rect (this.Bounds.SliceB 50.0f) Colors.shadow_2.O3
        base.Draw()

    override this.Update(elapsed_ms, moved) =
        if (%%"select").Pressed() then
            if chatline.Selected && current_message.Value <> "" then
                lobby.SendMessage current_message.Value
                current_message.Set ""
            else
                chatline.Select false

        base.Update(elapsed_ms, moved)