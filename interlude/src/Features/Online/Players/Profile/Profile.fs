namespace Interlude.Features.Online.Players

open System
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type ProfileData = Players.Profile.View.Response

type private ProfileSettingsPage(profile: ProfileData, profile_color: Setting<int32>) =
    inherit Page()

    let color_picker =
        DropdownWrapper(fun d -> Position.BorderB(min d.Height 500.0f + 2.0f * Style.PADDING).Shrink(Style.PADDING))

    let badges =
        seq {
            for b in profile.Badges do
                for c in b.Colors do
                    yield (b.Name, c)
        } |> Array.ofSeq

    let change_color_dropdown() =

        let save_color color =
            Players.Profile.Options.post (
                { Color = color },
                function
                | Some true -> GameThread.defer <| fun () -> profile_color.Set color
                | _ -> Notifications.error (%"notification.network_action_failed", "")
            )

        let dropdown =
            Dropdown
                {
                    Items = badges |> Seq.map (fun (label, color) -> color, label)
                    ColorFunc = fun c -> Color.FromArgb c, Colors.shadow_2
                    Setting = Setting.make save_color profile_color.Get
                }

        color_picker.Show dropdown
        dropdown.Focus false

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"profile_settings.color",
            Button(
                K profile.Username,
                change_color_dropdown,
                Align = Alignment.LEFT,
                TextColor = fun () -> Color.FromArgb(profile_color.Value), Colors.shadow_2
            )
            |+ color_picker
        )
            .Pos(0)
        :> Widget

    override this.Title = %"profile_settings"
    override this.OnClose() = ()

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

    let rerender (container: WebRequestContainer<_>) (data: ProfileData) =

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
        |+ Text(
            data.Username,
            Color = K(Color.FromArgb data.Color, Colors.shadow_2),
            Align = Alignment.LEFT,
            Position = Position.SliceT(80.0f).Shrink(45.0f, 5.0f)
        )
        |+ Text(
            String.concat ", " (data.Badges |> Seq.map (fun b -> b.Name)),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.ShrinkT(70.0f).SliceT(40.0f).Shrink(45.0f, 0.0f)
        )
        |+ Text(
            [
                DateTimeOffset
                    .FromUnixTimeMilliseconds(data.DateSignedUp)
                    .ToLocalTime()
                    .DateTime.ToShortDateString()
            ]
            %> "online.players.profile.playing_since",
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT,
            Position = Position.ShrinkT(125.0f).SliceT(45.0f).Shrink(45.0f, 0.0f)
        )
        |+ Text(
            %"online.players.profile.recent_scores",
            Color = K Colors.text,
            Align = Alignment.LEFT,
            Position = Position.ShrinkT(125.0f).SliceT(45.0f).Shrink(45.0f, 0.0f)
        )
        |+ RecentScores(data.RecentScores, Position = Position.ShrinkT(130.0f).Shrink(40.0f))

        // Friend button when not your profile
        |+ InlaidButton(
            (if data.IsMutualFriend then
                    %"online.players.profile.mutual_friend"
                else
                    %"online.players.profile.remove_friend"),
            remove_friend,
            (if data.IsMutualFriend then
                    Icons.HEART
                else
                    Icons.USER_MINUS),
            HoverText = %"online.players.profile.remove_friend",
            HoverIcon = Icons.USER_MINUS,
            UnfocusedColor =
                (if data.IsMutualFriend then
                        Colors.text_pink_2
                    else
                        Colors.text_red_2),
            Position = Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f)
        )
            .Conditional(fun () -> data.IsFriend)
        |+ InlaidButton(
            %"online.players.profile.add_friend",
            add_friend,
            Icons.USER_PLUS,
            UnfocusedColor = Colors.text_green_2,
            Position = Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f)
        )
            .Conditional(fun () -> not data.IsFriend && data.Username <> Network.credentials.Username)

        // Profile settings
        |+ InlaidButton(
            %"profile_settings",
            (fun () -> ProfileSettingsPage(data, profile_color).Show()),
            Icons.SETTINGS,
            Position = Position.ShrinkR(40.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f)
        )
            .Conditional(fun () -> data.Username = Network.credentials.Username && has_colors)

        // Invite button
        |+ InlaidButton(
            %"online.players.profile.invite_to_lobby",
            (fun () ->
                Network.lobby.Value.InvitePlayer(data.Username)
                Notifications.action_feedback (Icons.SEND, %"notification.lobby_invite_sent", data.Username)
            ),
            Icons.SEND,
            Position = Position.ShrinkR(380.0f).SliceT(InlaidButton.HEIGHT).SliceR(300.0f)
        )
            .Conditional(fun () -> Network.lobby.IsSome && data.Username <> Network.credentials.Username && (not (Network.lobby.Value.Players.ContainsKey data.Username)))
        :> Widget

    let container = WebRequestContainer<ProfileData>(load_profile, rerender)

    override this.Init(parent) =
        this
        |* container
        Players.player_changed <- container.Reload
        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds !*Palette.DARK_100
        base.Draw()