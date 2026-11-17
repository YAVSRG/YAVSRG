namespace Prelude.Gameplay.Rulesets

open Prelude.Charts

[<Struct>]
type CachedRuleset =
    {
        Ruleset: Ruleset
        Hash: string
    }

    static member Create(ruleset: Ruleset) : CachedRuleset =
        { Ruleset = ruleset; Hash = ruleset.CalculateHash() }

[<AutoOpen>]
module RulesetExtensions =

    let SC_J4 = SC.create(4)
    let SC_J4_HASH = SC_J4.CalculateHash()

    let private OSU_RULESETS_HASHES: CachedRuleset array =
        let rulesets = Array.zeroCreate 101
        let mutable unique: Map<string, Ruleset> = Map.empty

        for od = 0 to 100 do
            let ruleset = OsuMania.create (float32 od * 0.1f) OsuMania.NoMod
            let hash = ruleset.CalculateHash()

            match unique.TryFind(hash) with
            | Some existing -> rulesets.[od] <- { Hash = hash; Ruleset = existing }
            | None ->
                unique <- unique.Add(hash, ruleset)
                rulesets.[od] <- { Hash = hash; Ruleset = ruleset }

        rulesets

    let private QUAVER_RULESET: CachedRuleset =
        CachedRuleset.Create(Quaver.create(Quaver.Standard))

    let private WIFE3_RULESET = CachedRuleset.Create(Wife3.create(4))

    type Ruleset with
        static member DefaultRuleset: Ruleset = SC_J4
        static member DefaultRulesetHash: string = SC_J4_HASH

        static member GetNativeRuleset(origins: ChartOrigin seq) : CachedRuleset option =
            let mutable wife3_as_fallback = false

            match
                Seq.tryPick
                    (function
                    | ChartOrigin.Etterna _ ->
                        wife3_as_fallback <- true
                        None
                    | ChartOrigin.Osu osu ->
                        let idx = int(round(osu.SourceOD * 10.0f)) |> min 100 |> max 0
                        Some(OSU_RULESETS_HASHES.[idx])
                    | ChartOrigin.Quaver _ -> Some(QUAVER_RULESET))
                    origins
            with
            | None when wife3_as_fallback -> Some(WIFE3_RULESET)
            | otherwise -> otherwise
