namespace Interlude.Features.Online.Players

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type private Profile() =
    inherit Container(NodeType.None)

    let load_profile (container: WebRequestContainer<_>) =
        if Network.status = Network.Status.LoggedIn then
            match Players.current_player with
            | Some p ->
                Players.Profile.View.get (
                    p,
                    fun response ->
                        sync
                        <| fun () ->
                            match response with
                            | Some result -> container.SetData result
                            | None -> container.ServerError()
                )
            | None ->
                Players.Profile.View.get_me (fun response ->
                    sync
                    <| fun () ->
                        match response with
                        | Some result -> container.SetData result
                        | None -> container.ServerError()
                )
        else
            container.Offline()

    let rerender (container: WebRequestContainer<_>) (data: Players.Profile.View.Response) =
        let color_picker =
            SwapContainer(
                Position = Position.TrimRight(40.0f).TrimTop(70.0f).SliceRight(300.0f).SliceTop(500.0f)
            )

        let has_colors = data.Badges |> Seq.exists (fun b -> not (List.isEmpty b.Colors))
        
        let change_color_dropdown() =
            let badges =
                seq {
                    for b in data.Badges do
                        for c in b.Colors do
                            yield (b.Name, c)
                }

            let save_color color =
                Players.Profile.Options.post (
                    { Color = color },
                    function
                    | Some true -> sync <| fun () -> container.SetData({ data with Color = color })
                    | _ -> Notifications.error (%"notification.network_action_failed", "")
                )

            let dropdown =
                Dropdown
                    {
                        Items = badges |> Seq.map (fun (label, color) -> color, label)
                        ColorFunc = fun c -> Color.FromArgb c, Colors.shadow_2
                        OnClose = fun () -> color_picker.Current <- (Dummy())
                        Setting = Setting.make save_color (fun () -> data.Color)
                    }

            color_picker.Current <- dropdown
            dropdown.Focus false

        let remove_friend() =
            Friends.Remove.delete (
                data.Username,
                function
                | Some true ->
                    sync
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
                    sync
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
            Position = Position.SliceTop(80.0f).Margin(45.0f, 5.0f)
        )
        |+ Text(
            String.concat ", " (data.Badges |> Seq.map (fun b -> b.Name)),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimTop(70.0f).SliceTop(40.0f).Margin(45.0f, 0.0f)
        )
        |+ Text(
            sprintf
                "Player since %O"
                (DateTimeOffset
                    .FromUnixTimeMilliseconds(data.DateSignedUp)
                    .ToLocalTime()
                    .DateTime.ToShortDateString()),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT,
            Position = Position.TrimTop(125.0f).SliceTop(45.0f).Margin(45.0f, 0.0f)
        )
        |+ Text(
            %"online.players.profile.recent_scores",
            Color = K Colors.text,
            Align = Alignment.LEFT,
            Position = Position.TrimTop(125.0f).SliceTop(45.0f).Margin(45.0f, 0.0f)
        )
        |+ RecentScores(data.RecentScores, Position = Position.TrimTop(130.0f).Margin(40.0f))

        // Friend button when not your profile
        |+ Conditional(
            (fun () -> data.IsFriend),
            InlaidButton(
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
                Position = Position.TrimRight(40.0f).SliceTop(70.0f).SliceRight(300.0f)
            )
        )
        |+ Conditional(
            (fun () -> data.Username <> Network.credentials.Username && not data.IsFriend),
            InlaidButton(
                %"online.players.profile.add_friend",
                add_friend,
                Icons.USER_PLUS,
                UnfocusedColor = Colors.text_green_2,
                Position = Position.TrimRight(40.0f).SliceTop(70.0f).SliceRight(300.0f)
            )
        )
        // Color button on your profile
        |+ Conditional(
            (fun () -> has_colors && data.Username = Network.credentials.Username),
            InlaidButton(
                %"online.players.profile.change_color",
                change_color_dropdown,
                Icons.REFRESH_CCW,
                Position = Position.TrimRight(40.0f).SliceTop(70.0f).SliceRight(300.0f)
            )
        )
        |+ color_picker
        :> Widget

    let container = WebRequestContainer<Players.Profile.View.Response>(load_profile, rerender)

    override this.Init(parent) =
        this
        |* container
        Players.player_changed <- container.Reload
        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds !*Palette.DARK_100
        base.Draw()