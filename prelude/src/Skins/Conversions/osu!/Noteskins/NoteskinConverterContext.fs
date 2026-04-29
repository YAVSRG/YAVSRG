namespace Prelude.Skins.Conversions.Osu.Noteskins

open System
open Prelude.Skins.Conversions.Osu

type internal ColumnTextures =
    {
        Note: Result<Texture list, string list>
        Head: Result<Texture list, string list>
        Body: Result<Texture list, string list>
        Tail: Result<Texture list, string list>
    }
    member this.Fingerprint =
        Result.toOption this.Note,
        Result.toOption this.Head,
        Result.toOption this.Body,
        Result.toOption this.Tail
        
type NoteskinConverterContext =
    internal {
        SkinIni: SkinIni
        Version: decimal
        Keymode: int
        KeymodeSettings: Mania
        DefaultSettings: Mania
        
        Source: string
        Target: string
        
        mutable FlipHoldTail: bool
        mutable UseHoldTail: bool
        mutable SkipTailConversion: bool
        
        mutable StageTextures: bool

        mutable KeyReceptors: bool
        mutable ReceptorOffset: float32

        mutable ColumnLights: bool
        mutable ColumnLightsOffset: float32

        mutable JudgementLine: bool
        mutable JudgementLineScale: float32

        mutable NoteExplosionsScale: float32 option
        mutable HoldExplosionsScale: float32 option
            
        Colors: byte array
        ReceptorColors: int array array
        ColumnLightColors: int array array
    }
    
    static member Create(source: string, target: string, ini: SkinIni, keymode: int) : NoteskinConverterContext =
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
            
            FlipHoldTail = keymode_settings.NoteFlipWhenUpsideDownΔT.[0]
            UseHoldTail = true
            SkipTailConversion = false
            
            StageTextures = false

            KeyReceptors = false
            ReceptorOffset = 0.0f

            ColumnLights = false
            ColumnLightsOffset = 0.0f

            JudgementLine = false
            JudgementLineScale = 1.0f

            NoteExplosionsScale = None
            HoldExplosionsScale = None
            
            Colors = Array.zeroCreate 10
            ReceptorColors =
                [|
                    [|0; 2; 0|]; [|0; 1; 1; 0|]; [|0; 1; 2; 1; 0|];
                    [|0; 1; 0; 0; 1; 0|]; [|0; 1; 0; 2; 0; 1; 0|]; [|0; 1; 1; 0; 0; 1; 1; 0|];
                    [|0; 1; 0; 1; 2; 1; 0; 1; 0|]; [|0; 1; 2; 1; 0; 0; 1; 2; 1; 0|]
                |]
            ColumnLightColors =
                [|
                    [|0; 2; 0|]; [|0; 1; 1; 0|]; [|0; 1; 2; 1; 0|];
                    [|0; 1; 0; 0; 1; 0|]; [|0; 1; 0; 2; 0; 1; 0|]; [|0; 1; 1; 0; 0; 1; 1; 0|];
                    [|0; 1; 0; 1; 2; 1; 0; 1; 0|]; [|0; 1; 2; 1; 0; 0; 1; 2; 1; 0|]
                |]
        }