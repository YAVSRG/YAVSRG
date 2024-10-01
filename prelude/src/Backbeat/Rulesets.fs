namespace Prelude.Backbeat

open System.Collections.Generic
open Percyqaz.Data
open Prelude.Gameplay.Rulesets

[<Json.AutoCodec>]
type RulesetRepo =
    {
        Rulesets: Dictionary<string, Ruleset>
    }