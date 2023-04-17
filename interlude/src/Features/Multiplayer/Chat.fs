namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Scoring.Grading
open Interlude.Web.Shared
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Online

type Chat() =
    inherit StaticContainer(NodeType.None)

    let MESSAGE_HEIGHT = 40.0f
    
    let current_message = Setting.simple ""

    let chat_msg(sender: string, message: string) =
        let w = Text.measure(Style.baseFont, sender) * 0.6f * MESSAGE_HEIGHT
        StaticContainer(NodeType.None)
        |+ Text(sender, Color = K Colors.text_subheading, Position = Position.SliceLeft w, Align = Alignment.RIGHT)
        |+ Text(": " + message, Color = K Colors.text, Position = Position.TrimLeft w, Align = Alignment.LEFT)

    let messages = FlowContainer.Vertical<Widget>(MESSAGE_HEIGHT, Spacing = 2.0f)
    let message_box = ScrollContainer.Flow(messages, Position = Position.TrimBottom(60.0f).Margin(5.0f))
    let chatline = TextEntry(current_message, "none", Position = Position.SliceBottom(50.0f).Margin(5.0f))

    let mutable last_msg : Widget option = None
    let add_msg(w: Widget) =
        messages.Add w
        match last_msg with
        | Some m ->
            if m.Bounds.Top - message_box.RemainingScrollAnimation - message_box.Bounds.Bottom < 200.0f then
                message_box.Scroll infinityf
        | None -> ()
        last_msg <- Some w

    let game_end_report() =
        if Online.Multiplayer.replays.Keys.Count > 0 then
            add_msg (Text(sprintf "== Results for %s ==" SelectedChart.chart.Value.Title, Color = K Colors.text, Align = Alignment.CENTER))
            let scores = 
                Online.Multiplayer.replays.Keys
                |> Seq.map (fun username ->
                    let s = Online.Multiplayer.replays.[username]
                    s.Update(Time.infinity)
                    username, s
                )
                |> Seq.sortByDescending(fun (_, s) -> s.Value)
            let mutable place = 0
            for username, score in scores do
                place <- place + 1
                let color =
                    match place with
                    | 1 -> Color.Gold
                    | 2 -> Color.Silver
                    | 3 -> Color.DarkOrange
                    | _ -> Color.White
                let cmp =
                    let lamp = Lamp.calculate score.Ruleset.Grading.Lamps score.State
                    let grade = Grade.calculate score.Ruleset.Grading.Grades score.State
                    StaticContainer(NodeType.None)
                    |+ Text(sprintf "%i. %s" place username, Color = K (color, Colors.shadow_1), Align = Alignment.LEFT)
                    |+ Text(sprintf "%s" (score.FormatAccuracy()), Color = K (score.Ruleset.GradeColor grade, Colors.shadow_1), Align = Alignment.CENTER)
                    |+ Text(score.Ruleset.LampName lamp, Color = K (score.Ruleset.LampColor lamp, Colors.shadow_1), Align = 0.75f)
                    |+ Text(sprintf "%ix" (score.State.BestCombo), Color = K (score.Ruleset.LampColor lamp, Colors.shadow_1), Align = Alignment.RIGHT)
                add_msg cmp

    let countdown(reason, seconds) =
        let now = System.DateTime.Now
        let seconds_left() =
            let elapsed = (System.DateTime.Now - now).TotalSeconds |> System.Math.Floor |> int
            max 0 (seconds - elapsed)
        StaticContainer(NodeType.None)
        |+ Text(
            (fun () -> sprintf "%s %s: %i" Icons.countdown reason (seconds_left())),
            Color = (fun () -> if seconds_left() > 0 then Colors.text_green_2 else Colors.text_greyout),
            Align = Alignment.CENTER)
        |> add_msg

    override this.Init(parent) =
        this
        |+ chatline
        |+ Text((fun () -> if current_message.Value = "" then "Press ENTER to chat" else ""), Color = K Colors.text_subheading, Position = Position.SliceBottom(50.0f).Margin(5.0f), Align = Alignment.LEFT)
        |* message_box

        Network.Events.chat_message.Add (chat_msg >> add_msg)
        Network.Events.system_message.Add (fun msg -> add_msg (Text(msg, Align = Alignment.CENTER)))
        Network.Events.lobby_event.Add (fun (kind, data) ->
            let text, color =
                match (kind, data) with
                | LobbyEvent.Join, who -> sprintf "%s %s joined" Icons.login who, Colors.green_accent
                | LobbyEvent.Leave, who -> sprintf "%s %s left" Icons.logout who, Colors.red_accent
                | LobbyEvent.Host, who -> sprintf "%s %s is now host" Icons.star who, Colors.yellow_accent
                | LobbyEvent.Ready, who -> sprintf "%s %s is ready" Icons.ready who, Colors.green
                | LobbyEvent.NotReady, who -> sprintf "%s %s is not ready" Icons.not_ready who, Colors.pink
                | LobbyEvent.Invite, who -> sprintf "%s %s invited" Icons.invite who, Colors.cyan_accent
                | LobbyEvent.Generic, msg -> sprintf "%s %s" Icons.info msg, Colors.grey_1
                | _, msg -> msg, Colors.white
            add_msg (Text(text, Color = (fun () -> color, Colors.shadow_1), Align = Alignment.CENTER))
            )
        Network.Events.game_end.Add game_end_report
        Network.Events.join_lobby.Add (fun () -> messages.Clear())
        Network.Events.countdown.Add countdown

        base.Init parent

    override this.Draw() =
        Draw.rect(this.Bounds.TrimBottom 60.0f) Colors.shadow_2.O3
        Draw.rect(this.Bounds.SliceBottom 50.0f) Colors.shadow_2.O3
        base.Draw()

    override this.Update(elapsedTime, moved) =
        if (!|"select").Tapped() then
            if chatline.Selected && current_message.Value <> "" then 
                Lobby.chat current_message.Value
                current_message.Set ""
            else chatline.Select()

        base.Update(elapsedTime, moved)