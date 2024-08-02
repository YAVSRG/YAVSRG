namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude

type private PlayerListSidebar() =
    inherit Container(NodeType.None)

    let online = OnlineList()
    let friends = FriendList()
    let search = SearchList()

    let swap = SwapContainer(online, Position = Position.ShrinkT(50.0f).Shrink(40.0f))

    let button (label: string, cmp) =
        let button = Button(label, (fun () -> swap.Current <- cmp))

        FrameContainer(
            NodeType.Container(fun () -> Some button),
            Border = K Color.Transparent,
            Fill =
                fun () ->
                    if swap.Current = cmp then
                        !*Palette.DARK_100
                    else
                        Colors.black.O2
        )
        |+ button

    override this.Init(parent) =
        this
        |+ (GridFlowContainer(50.0f, 3, Position = Position.SliceT(50.0f))
            |+ button (%"online.players.online", online)
            |+ button (%"online.players.friends", friends)
            |+ button (%"online.players.search", search))
        |* swap

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.ShrinkT(50.0f)) !*Palette.DARK_100
        base.Draw()