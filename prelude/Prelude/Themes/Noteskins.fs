namespace Prelude.Data.Themes

open System.IO
open System.IO.Compression
open Prelude.Common
open Prelude.Gameplay.NoteColors

type Explosions =
    {
        Scale: float32
        FadeTime: float32
        ExpandAmount: float32
        ExplodeOnMiss: bool
        AnimationFrameTime: float
    }
    static member Default =
        {
            Scale = 1.0f
            FadeTime = 1.0f
            ExpandAmount = 0.15f
            ExplodeOnMiss = false
            AnimationFrameTime = 50.0
        }

type NoteskinConfig =
    {
        Name: string
        Author: string
        Version: string

        /// Enables rotation for notes. Set this to true if your notes are arrows/should rotate depending on which column they are in
        UseRotation: bool
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
        // todo: group every animation thing under an animations object
        /// ???
        ColumnLightTime: float32

        /// ???
        AnimationFrameTime: float
        /// Config for explosion animations
        Explosions: Explosions
    }
    static member Default =
        {
            Name = "Unnamed Noteskin"
            Author = "Unknown"
            Version = "1.0.0"
            UseRotation = false
            FlipHoldTail = true
            UseHoldTailTexture = true
            HoldNoteTrim = 0.0f
            PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
            PlayfieldAlignment = 0.5f, 0.5f
            DroppedHoldColor = Color.FromArgb(180, 180, 180, 180)
            ColumnWidth = 150.0f
            ColumnLightTime = 0.4f
            AnimationFrameTime = 200.0
            Explosions = Explosions.Default
            NoteColors = ColorConfig.Default
        }
    member this.Validate =
        { this with
            NoteColors = this.NoteColors.Validate
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