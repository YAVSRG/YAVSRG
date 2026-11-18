namespace Interlude.Content

open Prelude
open Prelude.Data.ChartDatabase
open Prelude.Gameplay.Rulesets
open Prelude.Data.Options
    
[<AutoOpen>]
module Rulesets =
    
    let rulesets: RulesetLibrary = RulesetLibrary(get_game_folder("Rulesets"))
    
    // todo: rulesets.CanDelete instead of exposing this
    let DEFAULT_RULESET_ID = "sc-j4"

type Rulesets =

    static member GetCurrent(chart_meta: ChartMeta option) : RulesetInfo =
        rulesets.GetCurrent(chart_meta)

    static member GetCurrent(chart_meta: ChartMeta) : RulesetInfo =
        rulesets.GetCurrent(chart_meta)
