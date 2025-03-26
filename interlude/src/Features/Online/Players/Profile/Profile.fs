namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type StatsHeader(data: ProfileData) =
    inherit Container(NodeType.None)

    let xp = data.Stats.XP
    let level = xp |> current_level
    let xp_to_next_level = (xp_for_level (level + 1) - xp_for_level level |> float32) / 1000.0f
    let current_xp = (xp - xp_for_level level |> float32) / 1000.0f

    let BAR_PC = 0.6f

    override this.Draw() =

        let bar = this.Bounds.SlicePercentL(BAR_PC).SliceB(25.0f, 20.0f).ShrinkX(20.0f)
        Render.rect (bar.Translate(5.0f, 5.0f)) Colors.black
        Render.rect bar !*Palette.DARKER
        Render.rect (bar.SlicePercentL(float32 current_xp / float32 xp_to_next_level)) !*Palette.MAIN

        base.Draw()

    override this.Init(parent: Widget): unit =
        this
        |+ Text(sprintf "%.1fk / %.1fk" current_xp xp_to_next_level)
            .Color(Colors.text_subheading)
            .Position(Position.SlicePercentL(BAR_PC).SliceB(60.0f, 35.0f).ShrinkX(20.0f))
            .Align(Alignment.RIGHT)
        |+ Text(sprintf "Level %i" level)
            .Align(Alignment.LEFT)
            .Position(Position.SlicePercentL(BAR_PC).SliceB(55.0f, 50.0f).ShrinkX(20.0f))
        |+ Text(
            [ (Timestamp.to_datetimeoffset data.DateSignedUp).ToLocalTime().DateTime.ToShortDateString() ]
            %> "online.players.profile.signed_up")
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.SlicePercentR(1.0f - BAR_PC).SliceT(40.0f).ShrinkX(20.0f))
        |+ Text(
            [ if data.Stats.LastUpdated = 0L then "--" else (Timestamp.to_datetimeoffset data.Stats.LastUpdated).ToLocalTime().DateTime.ToShortDateString() ]
            %> "online.players.profile.last_seen")
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.SlicePercentR(1.0f - BAR_PC).ShrinkT(40.0f).SliceT(40.0f).ShrinkX(20.0f))
        |* Text(
            [ if data.Stats.LastUpdated = 0L then "--" else format_long_time data.Stats.TotalPlaytime ]
            %> "online.players.profile.playtime")
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.SlicePercentR(1.0f - BAR_PC).SliceB(40.0f).ShrinkX(20.0f))
        base.Init parent

type private Profile() =
    inherit Container(NodeType.None)

    let load_profile (container: WebRequestContainer<_>) =
        if Network.status = Network.Status.LoggedIn then
            match Players.current_player with
            | Some p ->
                Players.Profile.View.get (
                    p,
                    fun response ->
                        GameThread.defer
                        <| fun () ->
                            match response with
                            | Some result -> container.SetData result
                            | None -> container.ServerError()
                )
            | None ->
                Players.Profile.View.get_me (fun response ->
                    GameThread.defer
                    <| fun () ->
                        match response with
                        | Some result -> container.SetData result
                        | None -> container.ServerError()
                )
        else
            container.Offline()

    let rerender (container: WebRequestContainer<_>) (data: ProfileData) : Widget =

        let has_colors = data.Badges |> Seq.exists (fun b -> not (List.isEmpty b.Colors))

        let profile_color = Setting.simple data.Color

        let remove_friend() =
            Friends.Remove.delete (
                data.Username,
                function
                | Some true ->
                    GameThread.defer
                    <| fun () ->
                        container.SetData(
                            { data with
                                IsFriend = false
                                IsMutualFriend = false
                            }
                        )

                        Players.update_friends_list ()
                | _ -> Notifications.error (%"notification.network_action_failed", "")
            )

        let add_friend() =
            Friends.Add.post (
                { User = data.Username },
                function
                | Some true ->
                    GameThread.defer
                    <| fun () ->
                        container.SetData({ data with IsFriend = true })
                        Players.update_friends_list ()
                | _ -> Notifications.error (%"notification.network_action_failed", "")
            )

        Container(NodeType.None)
        // Main details
        |+ Text(data.Username)
            .Color((fun () -> Color.FromArgb profile_color.Value, Colors.shadow_2))
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(70.0f).ShrinkX(45.0f).TranslateY(10.0f))
        |+ StatsHeader(data)
            .Position(Position.SliceT(70.0f, 120.0f).ShrinkX(20.0f))
        |+ Text(%"online.players.profile.recent_scores")
            .Color(Colors.text)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(180.0f, 45.0f).ShrinkX(40.0f))
        |+ RecentScores(data.RecentScores)
            .Position(Position.ShrinkT(190.0f).Shrink(40.0f))

        // Friend button when not your profile
        |+ InlaidButton(
            (if data.IsMutualFriend then
                    %"online.players.profile.mutual_friend"
                else
                    %"online.players.profile.remove_friend"),
            remove_friend
        )
            .Icon(
                if data.IsMutualFriend then
                    Icons.HEART
                else
                    Icons.USER_MINUS
            )
            .HoverText(%"online.players.profile.remove_friend")
            .HoverIcon(Icons.USER_MINUS)
            .TextColor(
                if data.IsMutualFriend then
                    Colors.text_pink_2
                else
                    Colors.text_red_2
            )
            .Position(Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f))
            .Conditional(fun () -> data.IsFriend)
        |+ InlaidButton(
            %"online.players.profile.add_friend",
            add_friend
        )
            .Icon(Icons.USER_PLUS)
            .TextColor(Colors.text_green_2)
            .Position(Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f))
            .Conditional(fun () -> not data.IsFriend && data.Username <> Network.credentials.Username)

        // Profile settings
        |+ InlaidButton(
            %"profile_settings",
            (fun () -> ProfileSettingsPage(data, profile_color).Show())
        )
            .Icon(Icons.SETTINGS)
            .Position(Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f))
            .Conditional(fun () -> data.Username = Network.credentials.Username && has_colors)

        // Invite button
        |+ InlaidButton(
            %"online.players.profile.invite_to_lobby",
            (fun () ->
                Network.lobby.Value.InvitePlayer(data.Username)
                Notifications.action_feedback (Icons.SEND, %"notification.lobby_invite_sent", data.Username)
            )
        )
            .Icon(Icons.SEND)
            .Position(Position.ShrinkR(380.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f))
            .Conditional(fun () -> Network.lobby.IsSome && data.Username <> Network.credentials.Username && (not (Network.lobby.Value.Players.ContainsKey data.Username)))
        :> Widget

    let container = WebRequestContainer<ProfileData>(load_profile, rerender)

    override this.Init(parent) =
        this
        |* container
        Players.player_changed <- container.Reload
        base.Init parent

    override this.Draw() =
        Render.rect_c this.Bounds (Quad.gradient_top_to_bottom !*Palette.DARK_100 Colors.shadow_2.O2)
        base.Draw()