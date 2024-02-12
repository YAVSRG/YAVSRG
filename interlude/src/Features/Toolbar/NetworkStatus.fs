namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Online

type NetworkStatus() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let area = this.Bounds.Shrink(30.0f, 0.0f).TrimBottom(15.0f)

        let text, color =
            match Network.status with
            | Network.NotConnected -> Icons.USER_X + "  Offline", Colors.grey_2
            | Network.Connecting -> Icons.GLOBE + "  Connecting..", Colors.grey_1
            | Network.ConnectionFailed -> Icons.SLASH + "  Offline", Colors.red_accent
            | Network.Connected -> Icons.GLOBE + "  Not logged in", Colors.green_accent
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
                Icons.USERS + "  In a lobby",
                area.Shrink(10.0f, 5.0f),
                Colors.text_subheading,
                Alignment.CENTER
            )

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match this.Dropdown with
        | Some d -> d.Update(elapsed_ms, moved)
        | None -> ()

        if Mouse.hover this.Bounds && Mouse.left_click () then
            this.ToggleDropdown()

        if
            not Toolbar.hidden
            && Network.status = Network.LoggedIn
            && (%%"player_list").Tapped()
        then
            PlayersPage().Show()

    member this.MenuItems: ((unit -> unit) * string) seq =
        match Network.status with
        | Network.NotConnected -> [ (fun () -> Network.connect ()), Icons.GLOBE + " Connect" ]
        | Network.Connecting -> [ ignore, Icons.SLASH + " Cancel" ]
        | Network.ConnectionFailed -> [  (fun () -> Network.connect ()), Icons.GLOBE + " Reconnect" ]
        | Network.Connected ->
            [
                fun () ->
                    if Network.credentials.Token <> "" then
                        Network.login_with_token ()
                    else
                        Menu.ShowPage LoginPage
                ,
                Icons.LOG_IN + " Log in"
            ]
        | Network.LoggedIn ->
            [
                fun () -> Screen.change Screen.Type.Lobby Transitions.Flags.Default |> ignore
                ,
                Icons.USERS + " Multiplayer"
                (fun () -> PlayersPage().Show()), Icons.SEARCH + " Players"
                Network.logout, Icons.LOG_OUT + " Log out"
            ]

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | None ->
            let d = 
                DropdownMenu { 
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
