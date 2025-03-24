namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Online.Players
open Interlude.Features.Multiplayer

type NetworkStatus() as this =
    inherit Container(NodeType.None)

    let retry_timer = Animation.Delay(10000.0)

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + this.Bounds.Height).ShrinkT(this.Bounds.Height).Shrink(Style.PADDING, 0.0f))

    override this.Init(parent) =
        this |* dropdown_wrapper
        base.Init parent

    override this.Draw() =
        let area = this.Bounds.ShrinkX(30.0f).SliceT(InlaidButton.HEIGHT)

        let text, color =
            match Network.status with
            | Network.NotConnected -> Icons.USER_X + "  " + %"network.connection.offline", Colors.grey_2
            | Network.Connecting -> Icons.GLOBE + "  " + %"network.connection.connecting" + "..", Colors.grey_1
            | Network.ConnectionFailed -> Icons.WIFI_OFF + "  " + %"network.connection.offline", Colors.red_accent
            | Network.Connected -> Icons.GLOBE + "  " + %"network.connection.not_logged_in", Colors.green_accent
            | Network.LoggedIn -> Icons.GLOBE + "  " + Network.credentials.Username, Colors.green_accent

        Render.rect area (Colors.shadow_1.O2)
        Text.fill_b (Style.font, text, area.Shrink(10.0f, 5.0f), (color, Colors.shadow_1), Alignment.CENTER)

        if Network.credentials.Host = "localhost" then
            Text.fill_b (Style.font, "LOCALHOST", this.Bounds.SliceB(20.0f), Colors.text, Alignment.CENTER)

        if Screen.current_type <> ScreenType.Lobby && Network.lobby.IsSome then
            let area = area.Translate(-300.0f, 0.0f)
            Render.rect area (Colors.shadow_1.O2)

            Text.fill_b (
                Style.font,
                Icons.USERS + "  " + %"network.multiplayer.in_lobby",
                area.Shrink(10.0f, 5.0f),
                Colors.text_subheading,
                Alignment.CENTER
            )

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Network.status = Network.NotConnected && not Network.kicked_no_reconnect then
            retry_timer.Update elapsed_ms

            if retry_timer.Complete then
                retry_timer.Reset()
                Network.connect ()

        if Mouse.hover this.Bounds && Mouse.left_clicked () then
            Selection.clear ()
            this.ToggleDropdown()

        if
            not Toolbar.hidden
            && Network.status = Network.LoggedIn
            && (%%"player_list").Pressed()
        then
            PlayerListPage().Show()

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
                fun () ->
                    match Network.lobby with
                    | Some l ->  Screen.change ScreenType.Lobby Transitions.Default |> ignore
                    | None -> LobbySelectPage().Show()
                , Icons.USERS + " " + %"network.multiplayer"
                (fun () -> PlayerListPage().Show()), Icons.SEARCH + " " + %"network.players"
                Network.logout, Icons.LOG_OUT + " " + %"network.logout"
            ]

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            DropdownMenu
                {
                    Items = this.MenuItems
                }
        )