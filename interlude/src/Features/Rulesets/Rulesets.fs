namespace Interlude.Features.Rulesets

open Prelude
open Percyqaz.Flux.UI
open Interlude.UI.Menu
open Interlude.Features.Rulesets.Browser

type InstallRulesetsPage() =
    inherit Page()

    override this.Content() =
        NavigationContainer.Column(Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
        |+ (
            GridFlowContainer(50.0f, 3, Position = Position.Row(70.0f, 50.0f))
            |+ Button(%"rulesets.create.sc", fun () -> SCRulesetPage().Show())
            |+ Button(%"rulesets.create.osu", fun () -> OsuRulesetPage().Show())
            |+ Button(%"rulesets.create.wife", fun () -> WifeRulesetPage().Show())
        )
        |+ RulesetSearch(Position = Position.TrimTop(230.0f))
        |+ Text("Game rulesets", Position = Position.SliceTop(60.0f), Align = Alignment.CENTER)
        |+ Text("Community rulesets", Position = Position.Row(150.0f, 60.0f), Align = Alignment.CENTER)
        :> Widget

    override this.Title = %"imports.rulesets"
    override this.OnClose() = ()