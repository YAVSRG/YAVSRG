namespace Prelude.Skins.Conversions.Osu.HUD

open System
open Prelude.Skins.Conversions.Osu
        
type HudConverterContext =
    internal {
        SkinIni: SkinIni
        Version: decimal
        Keymode: int
        KeymodeSettings: Mania
        DefaultSettings: Mania
        
        Source: string
        Target: string
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
        }