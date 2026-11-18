namespace Prelude.Gameplay.Rulesets

open System.Runtime.CompilerServices

[<Struct>]
type RulesetInfo =
    {
        Ruleset: Ruleset
        Hash: string
    }

    static member Create(ruleset: Ruleset) : RulesetInfo =
        { Ruleset = ruleset; Hash = ruleset.CalculateHash() }

    [<Extension>]
    static member ToInfo(ruleset: Ruleset) : RulesetInfo = RulesetInfo.Create(ruleset)
