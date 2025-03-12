namespace Prelude.Gameplay.Rulesets

open Prelude.Charts

[<AutoOpen>]
module DefaultRuleset =

    let SC_J4 = SC.create 4
    let SC_J4_HASH = Ruleset.hash SC_J4

module Rulesets =

    let get_native_ruleset (origins: ChartOrigin seq) : Ruleset option =
        let mutable wife3_as_fallback = false
        match
            Seq.tryPick
                (
                    function
                    | ChartOrigin.Etterna _ -> wife3_as_fallback <- true; None
                    | ChartOrigin.Osu osu -> Some (OsuMania.create osu.SourceOD OsuMania.NoMod)
                    | ChartOrigin.Quaver _ -> Some (Quaver.create Quaver.Standard)
                )
                origins
        with
        | None when wife3_as_fallback -> Some (Wife3.create 4)
        | otherwise -> otherwise