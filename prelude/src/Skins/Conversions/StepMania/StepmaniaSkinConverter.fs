namespace Prelude.Skins.Conversions.Stepmania

open System.IO
open System.Text.RegularExpressions
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins

module StepmaniaSkinConverter =

    let load_bmp f =
        use s = File.Open(f, FileMode.Open)
        Bitmap.from_stream false s |> Option.get

    type private Transform =
        | NoTransform
        | Stretch
        | Resize

    let private convert_to_noteskin (source: string) (target: string) (is_arrows: bool) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"

        Directory.CreateDirectory target |> ignore

        let image_file_names =
            Directory.EnumerateFiles source
            |> Seq.where (fun p -> Path.GetExtension(p).ToLower() = ".png")
            |> Seq.map Path.GetFileNameWithoutExtension
            |> Array.ofSeq

        let find_image_keywords (words: string array) (antiwords: string array) =
            let result, matches =
                image_file_names
                |> Seq.where (fun name -> Seq.forall (fun (w: string) -> name.Contains(w, System.StringComparison.InvariantCultureIgnoreCase) |> not) antiwords)
                |> Seq.map (fun name -> name, words |> Seq.where (fun w -> name.Contains(w, System.StringComparison.InvariantCultureIgnoreCase)) |> Seq.length)
                |> Seq.maxBy snd
            if matches >= words.Length / 2 then Some result else None

        let dim_regex = Regex("""[0-9]+x[0-9]+""")
        let detect_image_dimensions name =
            let res = dim_regex.Matches name
            if res.Count > 0 then
                let split = res.[0].Value.Split("x")
                int split.[0], int split.[1]
            else 1, 1

        let map_words_to_texture (words: string array) (antiwords: string array) (texture: string) (transform: Transform) : bool =
            match find_image_keywords words antiwords with
            | None ->
                Logging.Debug "no suitable match found for '%s'" texture
                false
            | Some image ->
                let columns, rows = detect_image_dimensions image

                let image = load_bmp (Path.Combine(source, image + ".png"))

                let transformed =
                    match transform with
                    | Stretch when (columns, rows) = (1, 1) ->
                        image.Mutate(fun img -> img.Resize(image.Width, image.Width) |> ignore)
                        image
                    | Resize when (columns, rows) = (1, 1) ->
                        image.Mutate(fun img -> img.Resize(image.Width, image.Width) |> ignore)
                        image
                    | _ -> image

                transformed.SaveAsPng(Path.Combine(target, TextureFileName.to_grid texture (columns, rows)))
                true

        let receptor = map_words_to_texture [|"Down"; "Go"; "Receptor"|] [||] "receptor" NoTransform
        let note = map_words_to_texture [|"Down"; "Tap"; "Note"|] [||] "note" NoTransform
        let holdhead = map_words_to_texture [|"Down"; "Hold"|] [|"Body"; "BottomCap"; "Explosion"|] "holdhead" NoTransform
        let holdbody = map_words_to_texture [|"Down"; "Hold"; "Body"|] [|"BottomCap"; "Explosion"|] "holdbody" Stretch
        let holdtail = map_words_to_texture [|"Down"; "Hold"; "BottomCap"|] [|"Body"; "Explosion"|] "holdtail" Resize
        let noteexplosion = map_words_to_texture [|"Down"; "Tap"; "Explosion"; "Dim"|] [|"Note"|] "noteexplosion" Resize
        let holdexplosion = map_words_to_texture [|"Down"; "Hold"; "Explosion"|] [|"Body"; "BottomCap"|] "holdexplosion" Resize

        let config: NoteskinConfig =
            { NoteskinConfig.Default with
                NoteColors =
                    { ColorConfig.Default with
                        Style = ColorScheme.DDR
                        UseGlobalColors = true
                    }
                FlipHoldTail = true
                HoldNoteTrim = 0.5f
                UseHoldTailTexture = holdtail
                UseRotation = is_arrows
                EnableColumnLight = false
                ReceptorStyle = ReceptorStyle.Receptors

                UseExplosions = noteexplosion && holdexplosion
            }

        JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config

    let convert_to_skin (source: string) (target: string) (is_arrows: bool) =
        convert_to_noteskin source (Path.Combine(target, "Noteskin")) is_arrows
        JSON.ToFile
            (Path.Combine(target, "skin.json"), false)
            {
                Name = Path.GetFileName source
                Author = "Unknown"
                Editor = None
            }