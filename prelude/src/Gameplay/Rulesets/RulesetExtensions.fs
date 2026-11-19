namespace Prelude.Gameplay.Rulesets

open Prelude.Charts

[<AutoOpen>]
module RulesetExtensions =

    let SC_J4 = SC.create(4).ToInfo()
    let QUAVER_STANDARD: RulesetInfo = Quaver.create(Quaver.Standard).ToInfo()
    let WIFE3_J4: RulesetInfo = Wife3.create(4).ToInfo()

    let private OSU_RULESETS: RulesetInfo array =
        let rulesets = Array.zeroCreate 101

        for od = 0 to 100 do
            rulesets.[od] <- OsuMania.nomod(float32 od * 0.1f).ToInfo()

        rulesets

    [<RequireQualifiedAccess>]
    type NativeRuleset =
        | OsuMania of od: float32
        | Wife3
        | Quaver

        member this.ToInfo() : RulesetInfo =
            match this with
            | OsuMania od ->
                let idx = int(round(od * 10.0f)) |> min 100 |> max 0
                OSU_RULESETS.[idx]
            | Wife3 -> WIFE3_J4
            | Quaver -> QUAVER_STANDARD

        static member GetNativeRuleset(origins: ChartOrigin seq) : NativeRuleset option =
            let mutable wife3_as_fallback = false

            match
                Seq.tryPick
                    (function
                    | ChartOrigin.Etterna _ ->
                        wife3_as_fallback <- true
                        None
                    | ChartOrigin.Osu osu -> Some(NativeRuleset.OsuMania(osu.SourceOD))
                    | ChartOrigin.Quaver _ -> Some(NativeRuleset.Quaver))
                    origins
            with
            | None when wife3_as_fallback -> Some(NativeRuleset.Wife3)
            | otherwise -> otherwise

        static member GetNativeOsuRuleset(origins: ChartOrigin seq) : NativeRuleset option =
            Seq.tryPick
                (function
                | ChartOrigin.Osu osu -> Some(NativeRuleset.OsuMania(osu.SourceOD))
                | _ -> None)
                origins
