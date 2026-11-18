namespace Interlude.Content

open Interlude.Features.Gameplay
open Prelude.Data.Library
open Prelude.Gameplay.Rulesets

[<AutoOpen>]
module temp =

    type Rulesets with
        static member current =
            Rulesets.rulesets.GetCurrentRuleset(SelectedChart.CACHE_DATA.Value).Ruleset

        static member GetCurrent(chart_meta: ChartMeta option) : RulesetInfo =
            Rulesets.rulesets.GetCurrentRuleset(chart_meta)

        static member GetCurrent(chart_meta: ChartMeta) : RulesetInfo =
            Rulesets.rulesets.GetCurrentRuleset(chart_meta)

        static member GetCurrentRuleset(chart_meta: ChartMeta) : Ruleset =
            Rulesets.rulesets.GetCurrentRuleset(chart_meta).Ruleset

        static member GetCurrentHash(chart_meta: ChartMeta) : string =
            Rulesets.rulesets.GetCurrentRuleset(chart_meta).Hash
