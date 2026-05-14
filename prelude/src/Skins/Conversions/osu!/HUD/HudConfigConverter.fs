namespace Prelude.Skins.Conversions.Osu.HUD

open System.IO
open Prelude
open Prelude.Skins.HudLayouts

module internal HudConfigConverter =

    let convert_hud_config(ctx: HudConverterContext) : unit =
        let config: HudConfig =
            { HudConfig.Default with
                JudgementMeterFrameTime = 16.7f<ms / rate>
                JudgementMeterUseTexture = ctx.JudgementTextures
                JudgementMeterCustomDisplay =
                    if ctx.JudgementTextures then
                        Map.ofSeq [6, Array.init 6 JudgementDisplayType.Texture]
                    else Map.empty

                ComboUseFont = ctx.ComboFontSpacing.IsSome
                ComboFontSpacing = ctx.ComboFontSpacing |> Option.defaultValue 0.0f

                AccuracyUseFont = ctx.AccuracyFont.IsSome
                AccuracyFontSpacing = ctx.AccuracyFont |> Option.map _.Spacing |> Option.defaultValue 0.0f
                AccuracyDotExtraSpacing = ctx.AccuracyFont |> Option.map _.DotExtraSpacing |> Option.defaultValue 0.0f
                AccuracyPercentExtraSpacing = ctx.AccuracyFont |> Option.map _.PercentExtraSpacing |> Option.defaultValue 0.0f

                JudgementCounterUseFont = ctx.JudgementCounterFont.IsSome
                JudgementCounterFontSpacing = ctx.JudgementCounterFont |> Option.map _.Spacing |> Option.defaultValue 0.0f
                JudgementCounterDotExtraSpacing = ctx.JudgementCounterFont |> Option.map _.DotExtraSpacing |> Option.defaultValue 0.0f
                JudgementCounterColonExtraSpacing = ctx.JudgementCounterFont |> Option.map _.ColonExtraSpacing |> Option.defaultValue 0.0f

                JudgementCounterCustomDisplay =
                    if ctx.JudgementCounterTextures then
                        Map.ofSeq [6, Array.init 6 Some]
                    else Map.empty

                ProgressMeterUseFont = ctx.ProgressMeterFont.IsSome
                ProgressMeterFontSpacing = ctx.ProgressMeterFont |> Option.map _.Spacing |> Option.defaultValue 0.0f
                ProgressMeterColonExtraSpacing = ctx.ProgressMeterFont |> Option.map _.ColonExtraSpacing |> Option.defaultValue 0.0f
                ProgressMeterPercentExtraSpacing = ctx.ProgressMeterFont |> Option.map _.PercentExtraSpacing |> Option.defaultValue 0.0f
            }

        JSON.ToFile (Path.Combine(ctx.Target, "hud.json"), false) config