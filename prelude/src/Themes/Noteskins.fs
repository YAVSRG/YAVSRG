namespace Prelude.Data.Themes

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common
open Prelude.Gameplay.NoteColors

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type ExplosionColors =
    | Column
    | Judgement

[<Json.AutoCodec(false)>]
type Explosions =
    {
        Scale: float32
        FadeTime: float32
        ExpandAmount: float32
        ExplodeOnMiss: bool
        AnimationFrameTime: float
        Colors: ExplosionColors
    }
    static member Default =
        {
            Scale = 1.0f
            FadeTime = 1.0f
            ExpandAmount = 0.15f
            ExplodeOnMiss = false
            AnimationFrameTime = 50.0
            Colors = ExplosionColors.Column
        }

[<Json.AutoCodec(false)>]
type NoteskinConfig =
    {
        Name: string
        Author: string
        Version: string

        /// Contains settings for the color scheme of notes
        NoteColors: ColorConfig

        /// Hold tail textures are oriented for upscroll. Set this to true if you want them to be flipped when not playing in downscroll mode.
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
        // todo: group every animation thing under an animations object
        /// ???
        ColumnLightTime: float32
        /// Set to false to disable column lighting when keys are pressed
        EnableColumnLight: bool

        /// ???
        AnimationFrameTime: float
        /// Config for explosion animations
        Explosions: Explosions

        /// Enables rotation for notes. Set this to true if your notes are arrows/should rotate depending on which column they are in
        /// Applies to receptors, notes and if UseHoldTailTexture is false it applies to tails too.
        UseRotation: bool

        /// Stores rotation infomation for notes. Only applied when UseRotation is true
        Rotations: float array array

    }
    static member Default =
        {
            Name = "Unnamed Noteskin"
            Author = "Unknown"
            Version = "1.0.0"
            FlipHoldTail = true
            UseHoldTailTexture = true
            HoldNoteTrim = 0.0f
            PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
            PlayfieldAlignment = 0.5f, 0.5f
            DroppedHoldColor = Color.FromArgb(255, 150, 150, 150)
            ColumnWidth = 150.0f
            ColumnSpacing = 0.0f
            ColumnLightTime = 0.4f
            EnableColumnLight = true
            AnimationFrameTime = 200.0
            Explosions = Explosions.Default
            NoteColors = ColorConfig.Default
            UseRotation = false
            Rotations = [|
                [|90.0; 0.0; 270.0|]
                [|90.0; 0.0; 180.0; 270.0|]
                [|45.0; 135.0; 0.0; 225.0; 315.0|]
                [|90.0; 135.0; 0.0; 180.0; 225.0; 270.0|]
                [|135.0; 90.0; 45.0; 0.0; 315.0; 270.0; 225.0|]
                [|90.0; 0.0; 180.0; 270.0; 90.0; 0.0; 180.0; 270.0|]
                // todo: agree on rotations for 9b (popn doesn't have any so maybe all 0s is standard)
                [|0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0|]
                [|45.0; 135.0; 0.0; 225.0; 315.0; 45.0; 135.0; 0.0; 225.0; 315.0|]
            |]
        }
    member this.Validate =
        { this with
            NoteColors = this.NoteColors.Validate
            Rotations =
                if this.Rotations.Length = 8 && Array.indexed this.Rotations |> Array.forall (fun (i, a) -> a.Length = 3 + i) then this.Rotations
                else
                    Logging.Error("Problem with noteskin: Rotations are not in the right format - Please use the ingame editor")
                    NoteskinConfig.Default.Rotations
        }

type Noteskin(storage) as this =
    inherit Storage(storage)
        
    let mutable config : NoteskinConfig = NoteskinConfig.Default
    do config <-
        match this.TryGetJson<NoteskinConfig> (true, "noteskin.json") with
        | Some data -> data.Validate
        | None -> failwith "noteskin.json was missing or didn't load properly"
        
    member this.Config
        with set conf = config <- conf; this.WriteJson (config, "noteskin.json")
        and get () = config
            
    member this.GetTexture (name: string) : (Bitmap * TextureConfig) option =
        match this.LoadTexture name with
        | Ok res -> res
        | Error err -> Logging.Error(sprintf "Error loading noteskin texture '%s': %s" name err.Message); None
        
    static member FromZipFile (file: string) = 
        let stream = File.OpenRead file
        new Noteskin(Zip (new ZipArchive(stream), Some file))
    static member FromZipStream (stream: Stream) = new Noteskin(Zip (new ZipArchive(stream), None))
    static member FromPath (path: string) = new Noteskin(Folder path)

module Noteskin =
    
    let (|OsuSkinArchive|OsuSkinFolder|InterludeSkinArchive|Unknown|) (path: string) =
        if Directory.Exists path then
            if File.Exists (Path.Combine(path, "skin.ini")) then OsuSkinFolder else Unknown
        else
            let s = Path.GetExtension(path).ToLower()
            match s with
            | ".isk" -> InterludeSkinArchive
            | ".osk" -> OsuSkinArchive
            | _ -> Unknown

    [<Json.AutoCodec>]
    type RepoEntry =
        {
            Name: string
            Preview: string
            Download: string
        }

    [<Json.AutoCodec>]
    type Repo =
        {
            Noteskins: RepoEntry list
        }