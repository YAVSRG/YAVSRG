namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open Prelude
open Prelude.Skins.Noteskins
open Prelude.Skins.Conversions.Osu

module internal NoteskinConfigConverter =
        
    let generate_noteskin_config(ctx: NoteskinConverterContext) : unit =
        
        let color_config: ColorConfig =
            { ColorConfig.Default with
                Style = ColorScheme.Column
                UseGlobalColors = false
            }

        color_config.Colors.[ctx.Keymode - 2] <- ctx.Colors

        let config: NoteskinConfig =
            { NoteskinConfig.Default with
                NoteColors = color_config
                FlipHoldTail = ctx.FlipHoldTail
                UseHoldTailTexture = ctx.UseHoldTail
                HoldNoteTrim = if ctx.SkipTailConversion then 1.5f else 0.0f
                PlayfieldColor = ctx.KeymodeSettings.ColourΔ.[0]
                ColumnWidth = 1080f / 480f * float32 ctx.KeymodeSettings.ColumnWidth.[0]
                AnimationFrameTime = 16.7f<ms / rate>
                UseRotation = ctx.IsArrows
                EnableStageTextures = ctx.StageTextures

                EnableColumnLight = ctx.ColumnLights
                ColumnLightColors = ctx.ColumnLightColors
                ColumnLightOffset = ctx.ColumnLightsOffset

                UseReceptors = true
                ReceptorStyle = if ctx.KeyReceptors then ReceptorStyle.Keys else ReceptorStyle.Receptors
                ReceptorColors = ctx.ReceptorColors
                ReceptorOffset = ctx.ReceptorOffset
                NotesUnderReceptors = not ctx.KeymodeSettings.KeysUnderNotes

                UseJudgementLine = ctx.JudgementLine
                JudgementLineScale = ctx.JudgementLineScale

                UseExplosions = ctx.NoteExplosionsScale.IsSome && ctx.HoldExplosionsScale.IsSome
                NoteExplosionSettings =
                    match ctx.NoteExplosionsScale with
                    | None -> NoteExplosionConfig.Default
                    | Some scale -> { NoteExplosionConfig.Default with UseBuiltInAnimation = false; Scale = scale }
                HoldExplosionSettings =
                    match ctx.HoldExplosionsScale with
                    | None -> HoldExplosionConfig.Default
                    | Some scale -> { HoldExplosionConfig.Default with Scale = scale }
            }

        JSON.ToFile (Path.Combine(ctx.Target, "noteskin.json"), false) config