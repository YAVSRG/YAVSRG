namespace Prelude.Skins.Noteskins

open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Skins

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ExplosionColors =
    | Note
    | Judgements

[<Json.AutoCodec(false)>]
type NoteExplosionConfig =
    {
        Scale: float32
        AnimationFrameTime: float32<ms / rate>
        Colors: ExplosionColors
        Offset: float32

        UseBuiltInAnimation: bool
        Duration: float32<ms / rate>
        ExpandAmount: float32
    }
    static member Default =
        {
            Scale = 1.0f
            AnimationFrameTime = 20.0f<ms / rate>
            Colors = ExplosionColors.Note
            Offset = 0.0f

            UseBuiltInAnimation = true
            Duration = 300.0f<ms / rate>
            ExpandAmount = 0.15f
        }

[<Json.AutoCodec(false)>]
type HoldExplosionConfig =
    {
        // todo: second scale, frametime, colors for release explosion IF people ask for it
        Scale: float32
        AnimationFrameTime: float32<ms / rate>
        Colors: ExplosionColors
        Offset: float32

        UseReleaseExplosion: bool
        ReleaseUseBuiltInAnimation: bool
        Duration: float32<ms / rate>
        ExpandAmount: float32
    }
    member this.UseBuiltInAnimation =
        not this.UseReleaseExplosion || this.ReleaseUseBuiltInAnimation

    static member Default =
        {
            Scale = 1.0f
            AnimationFrameTime = 20.0f<ms / rate>
            Colors = ExplosionColors.Note
            Offset = 0.0f

            UseReleaseExplosion = false
            ReleaseUseBuiltInAnimation = true
            Duration = 300.0f<ms / rate>
            ExpandAmount = 0.15f
        }

type ReceptorStyle =
    | Receptors = 0
    | Rotate = 0
    | Keys = 1
    | Flip = 1

[<Json.AutoCodec(false)>]
type NoteskinConfig =
    {
        /// Hold tail textures are oriented for upscroll. Set this to true if you want them to be flipped when playing in downscroll mode.
        /// Ignored if UseHoldTailTexture is true.
        FlipHoldTail: bool
        /// Set to false if you want to use the `holdhead` texture for hold tails too
        UseHoldTailTexture: bool
        /// Visually shortens hold notes by the given number of pixels
        HoldNoteTrim: float32
        /// Set to true to ensure hold notes can never have negative visual length due to HoldNoteTrim
        MinimumHoldNoteLength: bool
        /// Sets the color that hold notes should turn when they are not being held
        DroppedHoldColor: Color
        /// Millisecond duration of each frame on note/hold texture animations
        AnimationFrameTime: float32<ms / rate>

        /// Sets the color of the playfield behind notes
        PlayfieldColor: Color
        /// Sets the alignment of the playfield - 0.5, 0.5 lines up the middle of the playfield with the middle of the screen
        PlayfieldAlignment: float32 * float32
        /// Sets the width of columns, in pixels
        ColumnWidth: float32
        /// If true, game uses KeymodeSpecificColumnWidth instead of ColumnWidth for column width
        UseKeymodeSpecificColumnWidth: bool
        /// Stores column width information for keymodes. Only applied when KeymodeSpecificColumnWidth is true
        KeymodeSpecificColumnWidth: float32 array
        /// Sets the spacing of columns, in pixels
        ColumnSpacing: float32
        /// If true, game uses AdvancedColumnSpacing instead of ColumnSpacing for column spacing
        UseAdvancedColumnSpacing: bool
        /// Stores column spacing information for keymodes. Only applied when UseAdvancedColumnSpacing is true
        AdvancedColumnSpacing: float32 array array
        /// If true, spacing between columns is filled instead of having a gap
        FillColumnGaps: bool

        /// Contains settings for the color scheme of notes
        NoteColors: ColorConfig

        /// When true, enables `stageleft` and `stageright` textures drawn next to the playfield
        EnableStageTextures: bool

        /// Set to false to disable column lighting when keys are pressed
        EnableColumnLight: bool
        ColumnLightOffset: float32
        ColumnLightColors: int array array
        /// Milliseconds duration of column light animation
        ColumnLightDuration: float

        /// Enables explosion animations
        UseExplosions: bool
        /// Config for explosion animations
        NoteExplosionSettings: NoteExplosionConfig
        HoldExplosionSettings: HoldExplosionConfig

        /// Enables rotation for notes. Set this to true if your notes are arrows/should rotate depending on which column they are in
        /// Applies to receptors, notes and if UseHoldTailTexture is false it applies to tails too.
        UseRotation: bool
        /// Stores rotation infomation for notes. Only applied when UseRotation is true
        Rotations: float array array

        UseReceptors: bool
        ReceptorStyle: ReceptorStyle
        ReceptorOffset: float32
        ReceptorColors: int array array

        UseJudgementLine: bool
        JudgementLineScale: float32
        JudgementLineOffset: float32

        NotesUnderReceptors: bool

        /// When false, textures are upscaled pixel-for-pixel without blurring
        /// Should be false for skins with crisp pixel art edges, and true for skins with smooth edges that should stay smooth at high resolution
        LinearSampling: bool
    }
    static member Default =
        {
            FlipHoldTail = true
            UseHoldTailTexture = true
            HoldNoteTrim = 0.0f
            MinimumHoldNoteLength = false
            DroppedHoldColor = Color.FromArgb(255, 150, 150, 150)
            AnimationFrameTime = 200.0f<ms / rate>

            PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
            PlayfieldAlignment = 0.5f, 0.5f
            ColumnWidth = 150.0f
            UseKeymodeSpecificColumnWidth = false
            KeymodeSpecificColumnWidth =
                [|
                    150.0f
                    150.0f
                    150.0f
                    150.0f
                    150.0f
                    150.0f
                    150.0f
                    150.0f
                |]
            ColumnSpacing = 0.0f
            UseAdvancedColumnSpacing = false
            AdvancedColumnSpacing =
                [|
                    Array.zeroCreate 2
                    Array.zeroCreate 3
                    Array.zeroCreate 4
                    Array.zeroCreate 5
                    Array.zeroCreate 6
                    Array.zeroCreate 7
                    Array.zeroCreate 8
                    Array.zeroCreate 9
                |]
            FillColumnGaps = false

            NoteColors = ColorConfig.Default

            EnableStageTextures = false

            EnableColumnLight = false
            ColumnLightOffset = 0.0f
            ColumnLightColors =
                [|
                    Array.init 3 id
                    Array.init 4 id
                    Array.init 5 id
                    Array.init 6 id
                    Array.init 7 id
                    Array.init 8 id
                    Array.init 9 id
                    Array.init 10 id
                |]
            ColumnLightDuration = 100.0

            UseExplosions = false
            NoteExplosionSettings = NoteExplosionConfig.Default
            HoldExplosionSettings = HoldExplosionConfig.Default

            UseRotation = false
            Rotations =
                [|
                    [| 90.0; 0.0; 270.0 |]
                    [| 90.0; 0.0; 180.0; 270.0 |]
                    [| 45.0; 135.0; 0.0; 225.0; 315.0 |]
                    [| 90.0; 135.0; 0.0; 180.0; 225.0; 270.0 |]
                    [| 135.0; 90.0; 45.0; 0.0; 315.0; 270.0; 225.0 |]
                    [| 90.0; 0.0; 180.0; 270.0; 90.0; 0.0; 180.0; 270.0 |]
                    [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 |]
                    [| 45.0; 135.0; 0.0; 225.0; 315.0; 45.0; 135.0; 0.0; 225.0; 315.0 |]
                |]

            UseReceptors = true
            ReceptorStyle = ReceptorStyle.Receptors
            ReceptorOffset = 0.0f
            ReceptorColors =
                [|
                    Array.init 3 id
                    Array.init 4 id
                    Array.init 5 id
                    Array.init 6 id
                    Array.init 7 id
                    Array.init 8 id
                    Array.init 9 id
                    Array.init 10 id
                |]

            UseJudgementLine = false
            JudgementLineScale = 1.0f
            JudgementLineOffset = 0.0f

            NotesUnderReceptors = false

            LinearSampling = true
        }

    member this.Validate =
        { this with
            NoteColors = this.NoteColors.Validate
            Rotations =
                if
                    this.Rotations.Length = 8
                    && Array.indexed this.Rotations |> Array.forall (fun (i, a) -> a.Length = 3 + i)
                then
                    this.Rotations
                else
                    Logging.Error(
                        "Problem with noteskin: Rotations are not in the right format - Please use the ingame editor"
                    )

                    NoteskinConfig.Default.Rotations
            AdvancedColumnSpacing =
                if
                    this.AdvancedColumnSpacing.Length = 8
                    && Array.indexed this.AdvancedColumnSpacing
                       |> Array.forall (fun (i, a) -> a.Length = 2 + i)
                then
                    this.AdvancedColumnSpacing
                else
                    Logging.Error(
                        "Problem with noteskin: AdvancedColumnSpacing is not in the right format - Please use the ingame editor"
                    )

                    NoteskinConfig.Default.AdvancedColumnSpacing
            ColumnLightColors =
                if
                    this.ColumnLightColors.Length = 8
                    && Array.indexed this.ColumnLightColors |> Array.forall (fun (i, a) -> a.Length = 3 + i)
                then
                    this.ColumnLightColors
                else
                    Logging.Error(
                        "Problem with noteskin: ColumnLightColors are not in the right format - Please use the ingame editor"
                    )

                    NoteskinConfig.Default.ColumnLightColors
            ReceptorColors =
                if
                    this.ReceptorColors.Length = 8
                    && Array.indexed this.ReceptorColors |> Array.forall (fun (i, a) -> a.Length = 3 + i)
                then
                    this.ReceptorColors
                else
                    Logging.Error(
                        "Problem with noteskin: ReceptorColors are not in the right format - Please use the ingame editor"
                    )

                    NoteskinConfig.Default.ReceptorColors
        }

    member this.KeymodeColumnSpacing(keymode: int) : float32 array =
        if this.UseAdvancedColumnSpacing then
            this.AdvancedColumnSpacing.[keymode - 3]
        else
            Array.create (keymode - 1) this.ColumnSpacing

    member this.KeymodeColumnWidth(keymode: int) : float32 =
        if this.UseKeymodeSpecificColumnWidth then
            this.KeymodeSpecificColumnWidth.[keymode - 3]
        else
            this.ColumnWidth

    member this.DefaultColumnWidth = this.KeymodeColumnWidth 4

type NoteskinTextureRules =
    {
        IsRequired: NoteskinConfig -> bool
        MustBeSquare: NoteskinConfig -> bool
        MaxGridSize: NoteskinConfig -> int * int
    }
    member this.Evaluate(config: NoteskinConfig) : TextureRules =
        {
            IsRequired = this.IsRequired config
            MustBeSquare = this.MustBeSquare config
            MaxGridSize = this.MaxGridSize config
        }

module NoteskinTextureRules =

    let DEFAULT =
        {
            IsRequired = K true
            MustBeSquare = K true
            MaxGridSize = K(16, 32)
        }

    let TEXTURES: Map<string, NoteskinTextureRules> =
        Map.ofList
            [
                "note", DEFAULT
                "holdhead", DEFAULT
                "holdbody", DEFAULT
                "holdtail",
                { DEFAULT with
                    IsRequired = fun config -> config.UseHoldTailTexture
                }
                "receptor",
                {
                    IsRequired = fun config -> config.UseReceptors
                    MustBeSquare = fun config -> config.ReceptorStyle = ReceptorStyle.Receptors
                    MaxGridSize = K(20, 32)
                }
                "judgementline",
                {
                    IsRequired = fun config -> config.UseJudgementLine
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
                "noteexplosion",
                { DEFAULT with
                    IsRequired = fun config -> config.UseExplosions
                }
                "holdexplosion",
                { DEFAULT with
                    IsRequired = fun config -> config.UseExplosions
                }
                "releaseexplosion",
                { DEFAULT with
                    IsRequired = fun config -> config.UseExplosions && config.HoldExplosionSettings.UseReleaseExplosion
                }
                "receptorlighting",
                {
                    IsRequired = fun config -> config.EnableColumnLight
                    MustBeSquare = K false
                    MaxGridSize = K(10, 32)
                }
                "stageleft",
                {
                    IsRequired = fun config -> config.EnableStageTextures
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
                "stageright",
                {
                    IsRequired = fun config -> config.EnableStageTextures
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
            ]

    let get (config: NoteskinConfig) (name: string) : TextureRules = TEXTURES.[name].Evaluate config

    let list () : string seq = TEXTURES.Keys :> string seq