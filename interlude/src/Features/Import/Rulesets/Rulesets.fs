namespace Interlude.Features.Import

open Prelude
open Percyqaz.Flux.UI

module Rulesets =

    let tab =
        NavigationContainer.Column<Widget>()
        |+ (
            GridFlowContainer(50.0f, 3, Position = Position.Row(70.0f, 50.0f))
            |+ Button(%"ruleset.create.sc", fun () -> SCRulesetPage().Show())
            |+ Button(%"ruleset.create.osu", fun () -> OsuRulesetPage().Show())
            |+ Button(%"ruleset.create.wife", fun () -> WifeRulesetPage().Show())
        )
        |+ RulesetSearch(Position = Position.TrimTop(230.0f))
        |+ Text("Game rulesets", Position = Position.SliceTop(60.0f), Align = Alignment.CENTER)
        |+ Text("Community rulesets", Position = Position.Row(150.0f, 60.0f), Align = Alignment.CENTER)