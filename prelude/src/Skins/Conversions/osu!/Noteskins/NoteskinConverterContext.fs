namespace Prelude.Skins.Conversions.Osu.Noteskins

open System
open Prelude.Skins.Conversions.Osu

type internal ColumnTextures =
    {
        Note: TextureAnimationSearchResult
        Head: TextureAnimationSearchResult
        Body: TextureAnimationSearchResult
        Tail: TextureAnimationSearchResult
    }
    member this.Fingerprint =
        this.Note.ToOption(), this.Head.ToOption(), this.Body.ToOption(), this.Tail.ToOption()
        
type NoteskinConverterContext =
    internal {
        FileSystem: OsuSkinFileSystem
        SkinIni: SkinIni
        Version: decimal
        Keymode: int
        KeymodeSettings: Mania
        DefaultSettings: Mania
        
        Target: string
        
        IsArrows: bool
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
    
    static member Create(fs: OsuSkinFileSystem, target: string, ini: SkinIni, keymode: int, is_arrows: bool) : NoteskinConverterContext =
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
            
            FileSystem = fs
            Target = target
            
            IsArrows = is_arrows
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