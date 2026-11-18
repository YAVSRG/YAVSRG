namespace Interlude.Content

open Prelude
open Prelude.Data.Options
    
[<AutoOpen>]
module Rulesets =
    
    let rulesets: RulesetLibrary = RulesetLibrary(get_game_folder("Rulesets"))
    
    // todo: rulesets.CanDelete instead of exposing this
    let DEFAULT_RULESET_ID = "sc-j4"