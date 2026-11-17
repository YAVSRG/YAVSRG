namespace Interlude.Content

[<AutoOpen>]
module temp =

    type Rulesets with
        static member current = Rulesets.rulesets.Current
        static member current_hash = Rulesets.rulesets.CurrentHash
