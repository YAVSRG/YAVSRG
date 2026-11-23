namespace Interlude.Features.Online.Players

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude

type private PlayerButton(ctx: PlayerListContext, username: string, color: int) =
    inherit Container(NodeType.Button(fun () -> ctx.SwitchPlayer(username)))

    static member HEIGHT = 55.0f

    override this.Init(parent: Widget) : unit =
        this.Add(
            Text(username)
                .Color(Color.FromArgb(color), Colors.shadow_2)
                .Align(Alignment.LEFT)
                .Position(Position.Shrink(20.0f, 5.0f)),
            MouseListener().Button(this)
        )

        base.Init(parent)

    override this.Draw() : unit =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1
        else
            Render.rect this.Bounds Colors.shadow_2.O2

        base.Draw()
