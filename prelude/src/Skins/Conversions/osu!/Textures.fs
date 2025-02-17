namespace Prelude.Skins.Conversions.Osu

open System.IO
open Percyqaz.Common
open SixLabors.ImageSharp.Processing
open Prelude

type LoadedTexture =
    {
        Image: Bitmap
        Is2x: bool
    }
    member this.As2x : Bitmap =
        if this.Is2x then
            let new_image = this.Image.Clone()
            new_image.Mutate(fun img -> img.Resize(this.Image.Width * 2, 0) |> ignore)
            new_image
        else this.Image

type Texture =
    {
        Path: string
        Is2x: bool
    }

module Texture =

    let find_fallback(fallback_id: string, path: string) : Result<Texture, string list> =
        let file = Path.Combine(path, fallback_id)

        if File.Exists(file + "@2x.png") then Ok { Path = file + "@2x.png"; Is2x = true }
        elif File.Exists(file + ".png") then Ok { Path = file + ".png"; Is2x = false }
        else Error (file + "@2x.png" :: file + ".png" :: [])

    let find (requested_id: string, fallback_id: string, path: string) : Result<Texture, string list> =
        if requested_id = fallback_id then find_fallback(fallback_id, path) else

        let file = Path.Combine(path, requested_id)

        if File.Exists(file + "@2x.png") then Ok { Path = file + "@2x.png"; Is2x = true }
        elif File.Exists(file + ".png") then Ok { Path = file + ".png"; Is2x = false }
        else
            match find_fallback(fallback_id, path) with
            | Ok t -> Ok t
            | Error expected_files -> Error (file + "@2x.png" :: file + ".png" :: expected_files)

    let find_animated_fallback (fallback_id: string, path: string) : Result<Texture list, string list> =

        let file = Path.Combine(path, fallback_id)

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                { Path = file + "-" + i.ToString() + "@2x.png"; Is2x = true } :: find_animation (i + 1)
            elif File.Exists(file + "-" + i.ToString() + ".png") then
                { Path = file + "-" + i.ToString() + ".png"; Is2x = false } :: find_animation (i + 1)
            else []

        let animation = find_animation 0

        if animation <> [] then
            Ok animation
        elif File.Exists(file + "@2x.png") then
            Ok [ { Path = file + "@2x.png"; Is2x = true } ]
        elif File.Exists(file + ".png") then
            Ok [ { Path = file + ".png"; Is2x = false } ]
        else
            Error (file + "-0@2x.png" :: file + "-0.png" :: file + "@2x.png" :: file + ".png" :: [])

    let find_animated (requested_id: string, fallback_id: string, path: string) : Result<Texture list, string list> =
        if requested_id = fallback_id then find_animated_fallback(fallback_id, path) else

        let file = Path.Combine(path, requested_id)

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                { Path = file + "-" + i.ToString() + "@2x.png"; Is2x = true } :: find_animation (i + 1)
            elif File.Exists(file + "-" + i.ToString() + ".png") then
                { Path = file + "-" + i.ToString() + ".png"; Is2x = false } :: find_animation (i + 1)
            else []

        let animation = find_animation 0

        if animation <> [] then
            Ok animation
        elif File.Exists(file + "@2x.png") then
            Ok [ { Path = file + "@2x.png"; Is2x = true } ]
        elif File.Exists(file + ".png") then
            Ok [ { Path = file + ".png"; Is2x = false } ]
        else
            match find_animated_fallback(fallback_id, path) with
            | Ok ts -> Ok ts
            | Error expected_files -> Error (file + "-0@2x.png" :: file + "-0.png" :: file + "@2x.png" :: file + ".png" :: expected_files)

    let transparent_fallback() : LoadedTexture =
        { Image = new Bitmap(64, 64); Is2x = true }

    let load_single_texture (texture: Result<Texture, string list>) : LoadedTexture =
        let fallback() : LoadedTexture =
            Logging.Debug "As a fallback, you're getting a transparent texture"
            transparent_fallback()

        match texture with
        | Ok t ->
            try
                use s = File.Open(t.Path, FileMode.Open)
                { Image = Bitmap.from_stream false s |> Option.get; Is2x = t.Is2x }
            with err ->
                Logging.Debug "Couldn't load file '%s' (File not openable or isn't a valid image): %s" t.Path err.Message
                fallback()
        | Error expected_files ->
            Logging.Debug "Couldn't find any skin file matching: %s" (expected_files |> Seq.map (sprintf "'%s'") |> String.concat ", ")
            fallback()

    let load_animated_texture (textures: Result<Texture list, string list>) : LoadedTexture list =

        let load_texture (t: Texture) : LoadedTexture option =
            try
                use s = File.Open(t.Path, FileMode.Open)
                Some { Image = Bitmap.from_stream false s |> Option.get; Is2x = t.Is2x }
            with err ->
                Logging.Debug "Couldn't load file '%s' (File not openable or isn't a valid image): %s" t.Path err.Message
                None

        let fallback (ts: LoadedTexture option list) : LoadedTexture list =
            match List.tryPick id ts with
            | None ->
                Logging.Debug "As a fallback for 0 animation frames loading, you're getting a transparent texture"
                [ transparent_fallback() ]
            | Some default_texture ->

            ts
            |> List.map (
                function
                | Some texture -> texture
                | None ->
                    Logging.Debug "A missing animation frame is getting one of the other frames as a fallback"
                    default_texture
            )

        match textures with
        | Ok ts ->
            assert(List.isEmpty ts |> not)
            ts
            |> List.map load_texture
            |> fallback
        | Error expected_files ->
            Logging.Debug "Couldn't find any skin files matching: %s" (expected_files |> Seq.map (sprintf "'%s'") |> String.concat ", ")
            Logging.Debug "As a fallback, you're getting a transparent texture"
            [ transparent_fallback() ]

    let load_many_animated_textures (textures: Result<Texture list, string list> list) : LoadedTexture list list =

        let load_texture (t: Texture) : LoadedTexture option =
            try
                use s = File.Open(t.Path, FileMode.Open)
                Some { Image = Bitmap.from_stream false s |> Option.get; Is2x = t.Is2x }
            with err ->
                Logging.Debug "Couldn't load file '%s' (File not openable or isn't a valid image): %s" t.Path err.Message
                None

        let load_textures (textures: Result<Texture list, string list>) : LoadedTexture list option =

            let fallback (ts: LoadedTexture option list) : LoadedTexture list option =
                match List.tryPick id ts with
                | None -> None
                | Some default_texture ->

                ts
                |> List.map (
                    function
                    | Some texture -> texture
                    | None ->
                        Logging.Debug "A missing animation frame is getting one of the other frames as a fallback"
                        default_texture
                )
                |> Some

            match textures with
            | Ok ts ->
                assert(List.isEmpty ts |> not)
                ts
                |> List.map load_texture
                |> fallback
            | Error expected_files ->
                Logging.Debug "Couldn't find any skin files matching: %s" (expected_files |> Seq.map (sprintf "'%s'") |> String.concat ", ")
                None

        let fallback (ts: LoadedTexture list option list) : LoadedTexture list list =
            match List.tryPick id ts with
            | None ->
                Logging.Debug "As a fallback for 0 animated textures loading, you're getting transparent textures"
                List.map (fun _ -> [ transparent_fallback() ]) ts
            | Some default_texture ->

            ts
            |> List.map (
                function
                | Some texture -> texture
                | None ->
                    Logging.Debug "A missing texture is getting one of the others as a fallback"
                    default_texture
            )

        textures |> List.map load_textures |> fallback