namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Online
open Interlude.Utils

type NetworkStatus() =
    inherit StaticWidget(NodeType.None)

    let retry_timer = Animation.Delay(10000.0)

    override this.Draw() =
        let area = this.Bounds.Shrink(30.0f, 0.0f).TrimBottom(15.0f)

        let text, color =
            match Network.status with
            | Network.NotConnected -> Icons.USER_X + "  " + %"network.connection.offline", Colors.grey_2
            | Network.Connecting -> Icons.GLOBE + "  " + %"network.connection.connecting" + "..", Colors.grey_1
            | Network.ConnectionFailed -> Icons.WIFI_OFF + "  " + "network.connection.offline", Colors.red_accent
            | Network.Connected -> Icons.GLOBE + "  " + %"network.connection.not_logged_in", Colors.green_accent
            | Network.LoggedIn -> Icons.GLOBE + "  " + Network.credentials.Username, Colors.green_accent

        Draw.rect area (Colors.shadow_1.O2)
        Text.fill_b (Style.font, text, area.Shrink(10.0f, 5.0f), (color, Colors.shadow_1), Alignment.CENTER)

        if Network.credentials.Host = "localhost" then
            Text.fill_b (Style.font, "LOCALHOST", this.Bounds.SliceBottom(20.0f), Colors.text, Alignment.CENTER)

        if Screen.current_type <> Screen.Type.Lobby && Network.lobby.IsSome then
            let area = area.Translate(-300.0f, 0.0f)
            Draw.rect area (Colors.shadow_1.O2)

            Text.fill_b (
                Style.font,
                Icons.USERS + "  " + %"network.multiplayer.in_lobby",
                area.Shrink(10.0f, 5.0f),
                Colors.text_subheading,
                Alignment.CENTER
            )

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Network.status = Network.NotConnected then
            retry_timer.Update elapsed_ms

            if retry_timer.Complete then
                retry_timer.Reset()
                Network.connect ()

        match this.Dropdown with
        | Some d -> d.Update(elapsed_ms, moved)
        | None -> ()

        if Mouse.hover this.Bounds && Mouse.left_click () then
            Selection.clear ()
            this.ToggleDropdown()

        if
            not Toolbar.hidden
            && Network.status = Network.LoggedIn
            && (%%"player_list").Tapped()
        then
            PlayersPage().Show()

    member this.MenuItems: ((unit -> unit) * string) seq =
        match Network.status with
        | Network.NotConnected -> [ (fun () -> Network.connect ()), Icons.GLOBE + " " + %"network.connection.connect" ]
        | Network.Connecting -> [ ignore, Icons.SLASH + " " + %"network.connection.cancel" ]
        | Network.ConnectionFailed -> [ (fun () -> Network.connect ()), Icons.GLOBE + " " + %"network.connection.reconnect" ]
        | Network.Connected ->
            [
                fun () ->
                    if Network.credentials.Token <> "" then
                        Network.login_with_token ()
                    else
                        Menu.ShowPage LoginPage
                , Icons.LOG_IN + " " + %"network.login"
            ]
        | Network.LoggedIn ->
            [
                fun () -> Screen.change Screen.Type.Lobby Transitions.Flags.Default |> ignore
                , Icons.USERS + " " + %"network.multiplayer"
                (fun () -> PlayersPage().Show()), Icons.SEARCH + " " + %"network.players"
                Network.logout, Icons.LOG_OUT + " " + %"network.logout"
            ]

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | None ->
            let d =
                DropdownMenu
                    {
                        Items = this.MenuItems
                        OnClose = (fun () -> this.Dropdown <- None)
                    }

            d.Position <-
                Position
                    .SliceTop(d.Height + this.Bounds.Height)
                    .TrimTop(this.Bounds.Height)
                    .Margin(Style.PADDING, 0.0f)

            d.Init this
            this.Dropdown <- Some d

    member val Dropdown: DropdownMenu option = None with get, set
