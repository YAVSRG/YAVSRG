namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Skins

type AnchorPage(ctx: PositionerContext) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(
            %"hud.anchor.playfield_center",
            (fun () -> ctx.ChangeCurrentAnchor(true, Alignment.CENTER); Menu.Exit())
        )
            .Pos(0)
        |+ PageButton(
            %"hud.anchor.playfield_left",
            (fun () -> ctx.ChangeCurrentAnchor(true, Alignment.LEFT); Menu.Exit())
        )
            .Pos(2)
        |+ PageButton(
            %"hud.anchor.playfield_right",
            (fun () -> ctx.ChangeCurrentAnchor(true, Alignment.RIGHT); Menu.Exit())
        )
            .Pos(4)
        |+ PageButton(
            %"hud.anchor.screen_center",
            (fun () -> ctx.ChangeCurrentAnchor(false, Alignment.CENTER); Menu.Exit())
        )
            .Pos(7)
        |+ PageButton(
            %"hud.anchor.screen_left",
            (fun () -> ctx.ChangeCurrentAnchor(false, Alignment.LEFT); Menu.Exit())
        )
            .Pos(9)
        |+ PageButton(
            %"hud.anchor.screen_right",
            (fun () -> ctx.ChangeCurrentAnchor(false, Alignment.RIGHT); Menu.Exit())
        )
            .Pos(11)
        |+ CalloutCard(Help.Info("hud.anchor"))
            .Pos(15)
        :> Widget

    override this.Title = %"hud.anchor" + " - " + HudElement.name ctx.Selected.Value
    override this.OnClose() = ()