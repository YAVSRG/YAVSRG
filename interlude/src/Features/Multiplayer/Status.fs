namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

type LoginPage() as this =
    inherit Page()

    let username = Setting.simple ""

    let login() =
        Network.login(username.Value)

    let success(username) =
        Network.credentials.Username <- username
        Menu.Back()

    let handler = Network.Events.successful_login.Subscribe(success)

    do
        this.Content(
            column()
            |+ PrettySetting("login.username", TextEntry(username, "none")).Pos(200.0f)
            |+ PrettyButton("confirm.yes", login).Pos(300.0f)
        )

    override this.Title = N"login"
    override this.OnClose() = handler.Dispose()

type Status() =
    inherit StaticWidget(NodeType.None)

    override this.Init(parent: Widget) =
        base.Init parent
        if Network.target_ip.ToString() <> "0.0.0.0" then Network.connect()

    override this.Draw() =
        let area = this.Bounds.Shrink(30.0f, 0.0f).TrimBottom(15.0f)
        let text, color =
            match Network.status with
            | Network.NotConnected -> Icons.not_connected + "  Offline", Color.FromArgb(200, 200, 200)
            | Network.Connecting -> Icons.connecting + "  Connecting..", Color.FromArgb(255, 255, 160)
            | Network.ConnectionFailed -> Icons.connection_failed + "  Offline", Color.FromArgb(255, 160, 160)
            | Network.Connected -> Icons.connected + "  Not logged in", Color.FromArgb(160, 255, 160)
            | Network.LoggedIn -> Icons.connected + "  " + Network.username, Color.FromArgb(160, 255, 160)

        Draw.rect area (Color.FromArgb(100, 0, 0, 0))
        Text.drawFillB(Style.baseFont, text, area.Shrink(10.0f, 5.0f), (color, Color.Black), Alignment.CENTER)

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
                    if Network.credentials.Username <> "" then Network.login Network.credentials.Username
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
