namespace Interlude.Features.Rulesets

open Percyqaz.Flux.UI
open Prelude.Gameplay.Scoring
open Prelude.Gameplay.Rulesets
open Interlude.UI

type CompareRulesetsPage(ruleset_a: Ruleset, ruleset_b: Ruleset) =
    inherit Page()

    override this.Content() =
        FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)
            .With(Dummy(NodeType.Leaf))
            .With(seq {
                for accuracy in [|0.90; 0.91; 0.92; 0.93; 0.94; 0.95; 0.96; 0.97; 0.98; 0.99; 0.995|] do
                    yield
                        match RulesetComparison.compare accuracy ruleset_a ruleset_b with
                        | Some known_other_accuracy ->
                            Container(NodeType.None)
                                .With(
                                    Text(sprintf "%s ~=" (ruleset_a.FormatAccuracy accuracy))
                                        .Align(Alignment.LEFT)
                                        .Color(Grade.calculate ruleset_a.Grades accuracy |> ruleset_a.GradeColor),
                                    Text(sprintf "%s (%s - %s)" (ruleset_b.FormatAccuracy known_other_accuracy.Average) (ruleset_b.FormatAccuracy known_other_accuracy.Min) (ruleset_b.FormatAccuracy known_other_accuracy.Max))
                                        .Align(Alignment.RIGHT)
                                        .Color(Grade.calculate ruleset_b.Grades known_other_accuracy.Average |> ruleset_b.GradeColor)
                                )
                            :> Widget
                        | None ->
                            Container(NodeType.None)
                                .With(
                                    Text(sprintf "%s ~=" (ruleset_a.FormatAccuracy accuracy))
                                        .Align(Alignment.LEFT)
                                        .Color(Grade.calculate ruleset_a.Grades accuracy |> ruleset_a.GradeColor),
                                    Text("???")
                                        .Align(Alignment.RIGHT)
                                        .Color(ruleset_b.GradeColor -1)
                                )
            })
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).ExpandT(PAGE_ITEM_HEIGHT).SliceL(PAGE_ITEM_WIDTH))

    override this.Title = sprintf "%s vs %s" ruleset_a.Name ruleset_b.Name