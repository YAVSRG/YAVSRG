namespace Prelude.Skins.Conversions.osu

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts

type LoadedTexture =
    {
        Image: Bitmap
        Is2x: bool
    }

type FoundTexture =
    {
        Path: string
        Is2x: bool
    }
    member this.Load =
        {
            Image =
                use s = File.Open(this.Path, FileMode.Open)
                Bitmap.from_stream false s |> Option.get
            Is2x = this.Is2x
        }

module Texture =

    let find_fallback(fallback_id: string, path: string) : FoundTexture option =
        let file = Path.Combine(path, fallback_id)

        if File.Exists(file + "@2x.png") then Some { Path = file + "@2x.png"; Is2x = true }
        elif File.Exists(file + ".png") then Some { Path = file + ".png"; Is2x = false }
        else None

    let find (requested_id: string, fallback_id: string, path: string) : FoundTexture option =
        let file = Path.Combine(path, requested_id)

        if File.Exists(file + "@2x.png") then Some { Path = file + "@2x.png"; Is2x = true }
        elif File.Exists(file + ".png") then Some { Path = file + ".png"; Is2x = false }
        else find_fallback(fallback_id, path)

    let find_animated_fallback (fallback_id: string, path: string) : FoundTexture list option =

        let file = Path.Combine(path, fallback_id)

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                { Path = file + "-" + i.ToString() + "@2x.png"; Is2x = true } :: find_animation (i + 1)
            elif File.Exists(file + "-" + i.ToString() + ".png") then
                { Path = file + "-" + i.ToString() + ".png"; Is2x = false } :: find_animation (i + 1)
            else []

        let animation = find_animation 0

        if animation <> [] then
            Some animation
        elif File.Exists(file + "@2x.png") then
            Some [ { Path = file + "@2x.png"; Is2x = true } ]
        elif File.Exists(file + ".png") then
            Some [ { Path = file + ".png"; Is2x = false } ]
        else
            None

    let find_animated (requested_id: string, fallback_id: string, path: string) : FoundTexture list option =

        let file = Path.Combine(path, requested_id)

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                { Path = file + "-" + i.ToString() + "@2x.png"; Is2x = true } :: find_animation (i + 1)
            elif File.Exists(file + "-" + i.ToString() + ".png") then
                { Path = file + "-" + i.ToString() + ".png"; Is2x = false } :: find_animation (i + 1)
            else []

        let animation = find_animation 0

        if animation <> [] then
            Some animation
        elif File.Exists(file + "@2x.png") then
            Some [ { Path = file + "@2x.png"; Is2x = true } ]
        elif File.Exists(file + ".png") then
            Some [ { Path = file + ".png"; Is2x = false } ]
        else
            find_animated_fallback(fallback_id, path)

    let load_animation_frames (paths: FoundTexture list list) =
        paths |> List.map (fun row -> row |> List.map _.Load)