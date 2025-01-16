namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Online

type Username() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let area = this.Bounds.Shrink(30.0f, 0.0f).ShrinkB(15.0f)

        let username =
            if Network.credentials.Username <> "" then
                Network.credentials.Username
            else %"network.connection.not_logged_in"

        let icon, color =
            match Network.status with
            | Network.NotConnected
            | Network.Connecting
            | Network.ConnectionFailed -> Icons.USER_X, Colors.grey_2
            | Network.Connected
            | Network.LoggedIn -> Icons.GLOBE, Colors.green_accent

        Render.rect area (Colors.shadow_1.O2)
        Text.fill_b (Style.font, icon + "  " + username, area.Shrink(10.0f, 5.0f), (color, Colors.shadow_1), Alignment.CENTER)