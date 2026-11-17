namespace Interlude.Content

open Interlude.Features.Gameplay

[<AutoOpen>]
module temp =

    type Rulesets with
        static member current =
            Rulesets.rulesets.GetCurrentRuleset(SelectedChart.CACHE_DATA.Value).Ruleset

        static member current_hash =
            Rulesets.rulesets.GetCurrentRuleset(SelectedChart.CACHE_DATA.Value).Hash
