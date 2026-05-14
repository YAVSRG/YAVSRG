namespace Prelude.Skins.Conversions.Osu.HUD

open System
open Prelude.Skins.Conversions.Osu

type internal ConvertedFont =
    {
        Spacing: float32
        DotExtraSpacing: float32
        ColonExtraSpacing: float32
        PercentExtraSpacing: float32
    }
     
type HudConverterContext =
    internal {
        SkinIni: SkinIni
        Version: decimal
        Keymode: int
        KeymodeSettings: Mania
        DefaultSettings: Mania
        
        Source: string
        Target: string
        mutable ComboFontSpacing: float32 option
        mutable AccuracyFont: ConvertedFont option
        mutable ProgressMeterFont: ConvertedFont option
        mutable JudgementCounterFont: ConvertedFont option

        mutable JudgementTextures: bool
        mutable JudgementCounterTextures: bool
    }
    
    static member Create(source: string, target: string, ini: SkinIni, keymode: int) : HudConverterContext =
        let version =
            match Decimal.TryParse(ini.General.Version, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> 3.0m
            
        let default_settings = Mania.Default keymode version
        
        let keymode_settings = 
            ini.Mania
            |> List.tryFind (fun m -> m.Keys = keymode)
            |> Option.defaultValue default_settings
            
        {
            SkinIni = ini
            Version = version
            Keymode = keymode
            KeymodeSettings = keymode_settings
            DefaultSettings = default_settings
            
            Source = source
            Target = target
            ComboFontSpacing = None
            AccuracyFont = None
            ProgressMeterFont = None
            JudgementCounterFont = None

            JudgementTextures = false
            JudgementCounterTextures = false
        }