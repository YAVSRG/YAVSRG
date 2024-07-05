namespace Prelude.Skins.Noteskins

open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing
open Prelude.Skins
open Prelude.Skins.HudLayouts

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ExplosionColors =
    | Note
    | Judgements

[<Json.AutoCodec(false)>]
type NoteExplosionConfig =
    {
        Scale: float32
        AnimationFrameTime: float
        Colors: ExplosionColors
        Offset: float32

        UseBuiltInAnimation: bool
        Duration: float
        ExpandAmount: float32
    }
    static member Default =
        {
            Scale = 1.0f
            AnimationFrameTime = 50.0
            Colors = ExplosionColors.Note
            Offset = 0.0f

            UseBuiltInAnimation = true
            Duration = 300.0
            ExpandAmount = 0.15f
        }

[<Json.AutoCodec(false)>]
type HoldExplosionConfig =
    {
        // todo: second scale, frametime, colors for release explosion IF people ask for it
        Scale: float32
        AnimationFrameTime: float
        Colors: ExplosionColors
        Offset: float32

        UseReleaseExplosion: bool
        ReleaseUseBuiltInAnimation: bool
        Duration: float
        ExpandAmount: float32
    }
    member this.UseBuiltInAnimation =
        not this.UseReleaseExplosion || this.ReleaseUseBuiltInAnimation

    static member Default =
        {
            Scale = 1.0f
            AnimationFrameTime = 50.0
            Colors = ExplosionColors.Note
            Offset = 0.0f

            UseReleaseExplosion = false
            ReleaseUseBuiltInAnimation = true
            Duration = 300.0
            ExpandAmount = 0.15f
        }

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ReceptorStyle =
    | Rotate
    | Flip

[<Json.AutoCodec(false)>]
type NoteskinConfig =
    {
        /// Contains settings for the color scheme of notes
        NoteColors: ColorConfig

        /// Hold tail textures are oriented for upscroll. Set this to true if you want them to be flipped when playing in downscroll mode.
        FlipHoldTail: bool
        /// Set to false if you want to use the `holdhead` texture for hold tails too
        UseHoldTailTexture: bool
        /// Visually shortens hold notes by the given number of pixels
        HoldNoteTrim: float32
        /// Sets the color that hold notes should turn when they are not being held
        DroppedHoldColor: Color

        /// Sets the color of the playfield behind notes
        PlayfieldColor: Color
        /// Sets the alignment of the playfield - 0.5, 0.5 lines up the middle of the playfield with the middle of the screen
        PlayfieldAlignment: float32 * float32
        /// Sets the width of columns, in pixels
        ColumnWidth: float32
        /// Sets the spacing of columns, in pixels
        ColumnSpacing: float32
        /// If true, game uses AdvancedColumnSpacing instead of ColumnSpacing for column spacing
        UseAdvancedColumnSpacing: bool
        /// Stores column spacing information for keymodes. Only applied when UseAdvancedColumnSpacing is true
        AdvancedColumnSpacing: float32 array array
        /// If true, spacing between columns is filled instead of having a gap
        FillColumnGaps: bool

        /// When true, enables `stageleft` and `stageright` textures drawn next to the playfield
        EnableStageTextures: bool

        /// Milliseconds duration of column light animation
        ColumnLightDuration: float
        /// Set to false to disable column lighting when keys are pressed
        EnableColumnLight: bool

        /// Millisecond duration of each frame on note/hold texture animations
        AnimationFrameTime: float

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

        ReceptorStyle: ReceptorStyle

        LinearSampling: bool
    }
    static member Default =
        {
            FlipHoldTail = true
            UseHoldTailTexture = true
            HoldNoteTrim = 0.0f
            PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
            PlayfieldAlignment = 0.5f, 0.5f
            DroppedHoldColor = Color.FromArgb(255, 150, 150, 150)
            ColumnWidth = 150.0f
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
            EnableStageTextures = false
            ColumnLightDuration = 100.0
            EnableColumnLight = true
            AnimationFrameTime = 200.0
            UseExplosions = false
            NoteExplosionSettings = NoteExplosionConfig.Default
            HoldExplosionSettings = HoldExplosionConfig.Default
            NoteColors = ColorConfig.Default
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
            ReceptorStyle = ReceptorStyle.Rotate
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
        }

    member this.KeymodeColumnSpacing(keymode: int) : float32 array =
        if this.UseAdvancedColumnSpacing then
            this.AdvancedColumnSpacing.[keymode - 3]
        else
            Array.create (keymode - 1) this.ColumnSpacing

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
                    IsRequired = K true
                    MustBeSquare = fun config -> config.ReceptorStyle = ReceptorStyle.Rotate
                    MaxGridSize = K(20, 32)
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

    let get (config: NoteskinConfig) (name: string) = TEXTURES.[name].Evaluate config

    let list () : string seq = TEXTURES.Keys :> string seq
