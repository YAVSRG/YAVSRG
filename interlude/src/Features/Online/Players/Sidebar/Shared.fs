namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude

module private Players =

    let mutable current_player = None
    let mutable player_changed = ignore
    let mutable friends_changed = ignore

    let switch (player: string option) =
        current_player <- player
        player_changed ()

    let update_friends_list () = friends_changed ()

type private PlayerButton(username, color) =
    inherit Container(NodeType.Button(fun () -> Players.switch (Some username)))

    override this.Init(parent) =
        this
        |+ Text(
            username,
            Color = K(Color.FromArgb color, Colors.shadow_2),
            Align = Alignment.LEFT,
            Position = Position.Margin(20.0f, 5.0f)
        )
        |* Clickable.Focus this

        base.Init parent

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()