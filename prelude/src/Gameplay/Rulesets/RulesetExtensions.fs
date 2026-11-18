namespace Prelude.Gameplay.Rulesets

open Prelude.Charts

[<AutoOpen>]
module RulesetExtensions =

    let SC_J4 = SC.create(4).ToInfo()

    let private OSU_RULESETS: RulesetInfo array =
        let rulesets = Array.zeroCreate 101

        for od = 0 to 100 do
            rulesets.[od] <- OsuMania.nomod(float32 od * 0.1f).ToInfo()

        rulesets

    let private QUAVER_STANDARD: RulesetInfo = Quaver.create(Quaver.Standard).ToInfo()

    let private WIFE3_J4 = Wife3.create(4).ToInfo()

    type Ruleset with

        static member GetOsuRuleset(od: float32) : RulesetInfo =
            let idx = int(round(od * 10.0f)) |> min 100 |> max 0
            OSU_RULESETS.[idx]

        static member GetNativeRuleset(origins: ChartOrigin seq) : RulesetInfo option =
            let mutable wife3_as_fallback = false

            match
                Seq.tryPick
                    (function
                    | ChartOrigin.Etterna _ ->
                        wife3_as_fallback <- true
                        None
                    | ChartOrigin.Osu osu -> Some(Ruleset.GetOsuRuleset(osu.SourceOD))
                    | ChartOrigin.Quaver _ -> Some(QUAVER_STANDARD))
                    origins
            with
            | None when wife3_as_fallback -> Some(WIFE3_J4)
            | otherwise -> otherwise

        static member GetAutomaticOsuRuleset(origins: ChartOrigin seq, default_od: float32) : RulesetInfo option =
            Seq.tryPick
                (function
                | ChartOrigin.Osu osu -> Some(Ruleset.GetOsuRuleset(osu.SourceOD))
                | _ -> None)
                origins
