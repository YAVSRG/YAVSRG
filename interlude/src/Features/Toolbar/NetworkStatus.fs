namespace Interlude.Features.Toolbar

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
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
            | Network.NotConnected -> Icons.not_connected + "  Offline", Colors.grey_2
            | Network.Connecting -> Icons.connecting + "  Connecting..", Colors.grey_1
            | Network.ConnectionFailed -> Icons.connection_failed + "  Offline", Colors.red_accent
            | Network.Connected -> Icons.connected + "  Not logged in", Colors.green_accent
            | Network.LoggedIn -> Icons.connected + "  " + Network.username, Colors.green_accent

        Draw.rect area (Colors.shadow_1.O2)
        Text.drawFillB(Style.baseFont, text, area.Shrink(10.0f, 5.0f), (color, Colors.shadow_1), Alignment.CENTER)
        if Network.credentials.Host = "localhost" then Text.drawFillB(Style.baseFont, "LOCALHOST", this.Bounds.SliceBottom(20.0f), Colors.text, Alignment.CENTER)

        if Screen.currentType <> Screen.Type.Lobby && Network.lobby.IsSome then
            let area = area.Translate(-300.0f, 0.0f)
            Draw.rect area (Colors.shadow_1.O2)
            Text.drawFillB(Style.baseFont, Icons.multiplayer + "  In a lobby", area.Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.CENTER)

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()

        if Mouse.hover this.Bounds && Mouse.leftClick() then this.ToggleDropdown()

    member this.MenuItems : (string * (unit -> unit)) seq =
        match Network.status with
        | Network.NotConnected -> [ Icons.connecting + " Connect", fun () -> Network.connect() ]
        | Network.Connecting -> [ Icons.connection_failed + " Cancel", ignore ]
        | Network.ConnectionFailed -> [ Icons.connecting + " Reconnect", fun () -> Network.connect() ]
        | Network.Connected -> [ 
                Icons.login + " Log in", 
                fun () -> 
                    if Network.credentials.Token <> "" then Network.login_with_token()
                    else Menu.ShowPage LoginPage
            ]
        | Network.LoggedIn -> [
                Icons.multiplayer + " Multiplayer", fun () -> Screen.change Screen.Type.Lobby Transitions.Flags.Default
                Icons.logout + " Log out", Network.logout
            ]

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | None ->
            let d = Dropdown(this.MenuItems, (fun () -> this.Dropdown <- None))
            d.Position <- Position.SliceTop(d.Height + Screen.Toolbar.HEIGHT).TrimTop(Screen.Toolbar.HEIGHT).Margin(Style.padding, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown : Dropdown option = None with get, set
