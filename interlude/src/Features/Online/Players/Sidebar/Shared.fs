namespace Interlude.Features.Online.Players

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

type private PlayerButton(username: string, color: int) =
    inherit Container(NodeType.Button(fun () -> Players.switch (Some username)))

    static member HEIGHT = 55.0f

    override this.Init(parent: Widget) =
        this
            .Add(
                Text(username)
                    .Color(Color.FromArgb color, Colors.shadow_2)
                    .Align(Alignment.LEFT)
                    .Position(Position.Shrink(20.0f, 5.0f)),
                MouseListener().Button(this)
            )

        base.Init parent

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1
        else
            Render.rect this.Bounds Colors.shadow_2.O2

        base.Draw()