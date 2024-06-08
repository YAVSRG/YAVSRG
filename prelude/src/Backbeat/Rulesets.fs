namespace Prelude.Backbeat

open System.Collections.Generic
open Percyqaz.Data
open Prelude.Gameplay

[<Json.AutoCodec>]
type RulesetRepo =
    {
        Rulesets: Dictionary<string, Ruleset>
    }