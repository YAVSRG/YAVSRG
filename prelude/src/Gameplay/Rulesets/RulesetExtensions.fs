namespace Prelude.Gameplay.Rulesets

open Prelude.Charts

[<AutoOpen>]
module RulesetExtensions =

    let SC_J4 = SC.create(4).ToInfo()

    let private OSU_RULESETS: RulesetInfo array =
        let rulesets = Array.zeroCreate 101
        let mutable unique: Map<string, Ruleset> = Map.empty

        for od = 0 to 100 do
            let ruleset = OsuMania.nomod(float32 od * 0.1f)
            let hash = ruleset.CalculateHash()

            match unique.TryFind(hash) with
            | Some existing -> rulesets.[od] <- { Hash = hash; Ruleset = existing }
            | None ->
                unique <- unique.Add(hash, ruleset)
                rulesets.[od] <- { Hash = hash; Ruleset = ruleset }

        rulesets

    let private QUAVER_STANDARD: RulesetInfo = Quaver.create(Quaver.Standard).ToInfo()

    let private WIFE3_J4 = Wife3.create(4).ToInfo()

    type Ruleset with
        static member GetNativeRuleset(origins: ChartOrigin seq) : RulesetInfo option =
            let mutable wife3_as_fallback = false

            match
                Seq.tryPick
                    (function
                    | ChartOrigin.Etterna _ ->
                        wife3_as_fallback <- true
                        None
                    | ChartOrigin.Osu osu ->
                        let idx = int(round(osu.SourceOD * 10.0f)) |> min 100 |> max 0
                        Some(OSU_RULESETS.[idx])
                    | ChartOrigin.Quaver _ -> Some(QUAVER_STANDARD))
                    origins
            with
            | None when wife3_as_fallback -> Some(WIFE3_J4)
            | otherwise -> otherwise
