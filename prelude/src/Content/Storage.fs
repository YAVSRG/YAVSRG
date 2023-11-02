namespace Prelude.Data.Content

open System
open System.IO
open System.IO.Compression
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common

(*
    Basic theme I/O stuff. Additional implementation in Interlude for texture-specific things that depend on Interlude
*)

[<Json.AutoCodec>]
type TextureFileMode =
    | Grid
    | Loose

[<Json.AutoCodec(false)>]
type TextureConfig =
    {
        Columns: int
        Rows: int
        Mode: TextureFileMode
    }
    static member Default = { Columns = 1; Rows = 1; Mode = Grid }

type StorageType =
    | Embedded of ZipArchive
    | Folder of string
    override this.ToString() =
        match this with
        | Embedded _ -> "[Embedded Assets]"
        | Folder f -> Path.GetFileName f + "/"

type Storage(storage: StorageType) =

    let mutable texture_config_cache: Map<string, TextureConfig> = Map.empty

    member this.Source = storage

    member this.IsEmbedded =
        match storage with
        | Embedded _ -> true
        | _ -> false

    /// Returns stream for requested file, or None if that file doesn't exist
    /// Can throw exceptions for other IO errors
    member this.TryReadFile([<ParamArray>] path: string array) : Stream option =
        let p = Path.Combine(path)

        try
            match storage with
            | Embedded z -> z.GetEntry(p.Replace(Path.DirectorySeparatorChar, '/')).Open() |> Some
            | Folder f ->
                let p = Path.Combine(f, p)
                File.OpenRead p :> Stream |> Some
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException // File doesnt exist in folder storage
        | :? NullReferenceException -> None // File doesnt exist in zip storage
        | :? IOException as err ->
            Logging.Error(
                sprintf
                    "IO error reading '%s' in: %O\n  This is unusual, maybe you still have the file open in another program?"
                    (String.concat "/" path)
                    storage,
                err
            )

            reraise ()
        | _ -> reraise ()

    /// Returns string names of files in the requested folder
    member this.GetFiles([<ParamArray>] path: string array) =
        let p = Path.Combine(path)

        match storage with
        | Embedded z ->
            let p = p.Replace(Path.DirectorySeparatorChar, '/')

            seq {
                for e in z.Entries do
                    if e.FullName = p + "/" + e.Name && Path.HasExtension e.Name then
                        yield e.Name
            }
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateFiles target |> Seq.map Path.GetFileName

    /// Returns string names of folders in the requested folder
    member this.GetFolders([<ParamArray>] path: string array) =
        let p = Path.Combine path

        match storage with
        | Embedded z ->
            let p = p.Replace(Path.DirectorySeparatorChar, '/')

            seq {
                for e in z.Entries do
                    if e.Name = "" && e.FullName.Length > p.Length then
                        let s = e.FullName.Substring(p.Length + 1).Split('/')

                        if e.FullName = p + "/" + s.[0] + "/" then
                            yield s.[0]
            }
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateDirectories target |> Seq.map Path.GetFileName

    /// Gets the specified JSON file, or returns None
    /// None is returned with silent fail if the file did not exist
    /// None is returned with an error logged if something goes wrong while reading the file
    member this.TryGetJson<'T>(write_back: bool, [<ParamArray>] path: string array) : 'T option =
        let json =
            match this.TryReadFile path with
            | Some stream ->
                use tr = new StreamReader(stream)

                let result =
                    tr.ReadToEnd()
                    |> JSON.FromString<'T>
                    |> function
                        | Ok v -> Some v
                        | Error err ->
                            Logging.Error(sprintf "Failed to load %s in: %O" (String.concat "/" path) storage, err)
                            None

                stream.Dispose()
                result
            | None -> None

        if write_back && json.IsSome then
            this.WriteJson<'T>(json.Value, path)

        json

    /// Gets the specified JSON file, or returns the default instance
    /// File is not written back if there was a problem with an existing file, but is if the file didn't exist
    member this.GetJsonOrDefault<'T>(write_back: bool, [<ParamArray>] path: string array) : 'T =
        let json, error_within_file =
            match this.TryReadFile path with
            | Some stream ->
                use tr = new StreamReader(stream)

                let result =
                    tr.ReadToEnd()
                    |> JSON.FromString<'T>
                    |> function
                        | Ok v -> v, false
                        | Error err ->
                            Logging.Error(
                                sprintf
                                    "JSON error reading '%s' in: %O\n  Data was wrong or not formatted properly (Check you aren't missing a comma)\n  Using default values instead"
                                    (String.concat "/" path)
                                    storage,
                                err
                            )

                            JSON.Default<'T>(), true

                stream.Dispose()
                result
            | None -> JSON.Default<'T>(), false

        if write_back && not error_within_file then
            this.WriteJson(json, path)

        json

    member this.WriteJson<'T>(data: 'T, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> () // Zip archive is read-only
        | Folder f ->
            let target = Path.Combine(f, Path.Combine path)
            target |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
            JSON.ToFile (target, true) data

    member this.ExtractToFolder target_directory =
        Directory.CreateDirectory target_directory |> ignore

        match storage with
        | Embedded z -> z.ExtractToDirectory target_directory
        | Folder _ -> failwith "Can only extract zip to folder"

    member this.CompressToZip target : bool =
        if File.Exists target then
            File.Delete target

        match storage with
        | Embedded _ ->
            Logging.Error("Exporting already zipped content as an archive is not implemented")
            false
        | Folder f ->
            try
                ZipFile.CreateFromDirectory(f, target)
                true
            with
            | :? DirectoryNotFoundException as e ->
                Logging.Error(
                    "Couldn't export archive because the folder moved. Did you move it while the game was open?",
                    e
                )

                false
            | :? IOException as e ->
                Logging.Error("IO exception while exporting archive", e)
                false
            | _ -> reraise ()

    // Texture loading

    member private this.GetTextureConfig(name: string, [<ParamArray>] path: string array) =
        let id = String.concat "/" path + "/" + name

        if not (texture_config_cache.ContainsKey id) then
            let info: TextureConfig =
                this.GetJsonOrDefault(false, Array.append path [| name + ".json" |])

            texture_config_cache <- texture_config_cache.Add(id, info)

        texture_config_cache.[id]

    member private this.WriteTextureConfig(info: TextureConfig, name: string, path: string array) =
        let id = String.concat "/" path + "/" + name
        texture_config_cache <- texture_config_cache.Add(id, info)
        this.WriteJson<TextureConfig>(info, Array.append path [| name + ".json" |])

    member private this.LoadGridTexture(config: TextureConfig, name: string, path: string array) : Bitmap option =
        if config.Columns < 1 then
            failwith "Columns must be a positive number"
        elif config.Rows < 1 then
            failwith "Rows must be a positive number"

        match this.TryReadFile(Array.append path [| name + ".png" |]) with
        | Some stream ->
            let img = Bitmap.load stream
            stream.Dispose()
            Some img
        | None -> None

    member private this.LoadLooseTextures(config: TextureConfig, name: string, path: string array) : Bitmap option =
        if config.Columns < 1 then
            failwith "Columns must be a positive number"
        elif config.Rows < 1 then
            failwith "Rows must be a positive number"

        let load_img row column =
            match this.TryReadFile(Array.append path [| sprintf "%s-%i-%i.png" name row column |]) with
            | Some stream ->
                let i = Bitmap.load stream in
                stream.Dispose()
                i
            | None -> failwithf "Couldn't load texture file (%i,%i)" row column

        let base_img = load_img 0 0

        if base_img.Height <> base_img.Width then
            failwith "Textures must be square"

        let atlas =
            new Bitmap(base_img.Width * config.Columns, base_img.Height * config.Rows)

        for row in 0 .. config.Rows - 1 do
            for col in 0 .. config.Columns - 1 do
                use img = if (row, col) = (0, 0) then base_img else load_img row col

                if img.Height <> base_img.Height || img.Width <> base_img.Width then
                    failwithf "All images must be the same dimensions, (%i, %i) doesn't match (0, 0)" row col

                atlas.Mutate<PixelFormats.Rgba32>(fun context ->
                    context.DrawImage(img, Point(col * base_img.Width, row * base_img.Height), 1.0f)
                    |> ignore
                )

        Some atlas

    member internal this.LoadTexture
        (
            name: string,
            [<ParamArray>] path: string array
        ) : Result<(Bitmap * TextureConfig) option, exn> =
        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid ->
            try
                match this.LoadGridTexture(info, name, path) with
                | Some img -> Ok(Some(img, info))
                | None -> Ok None
            with err ->
                Error err
        | Loose ->
            try
                match this.LoadLooseTextures(info, name, path) with
                | Some img -> Ok(Some(img, info))
                | None -> Ok None
            with err ->
                Error err

    member this.SplitTexture(name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->
            let info: TextureConfig = this.GetTextureConfig(name, path)

            match info.Mode with
            | Grid ->
                let img = Option.get (this.LoadGridTexture(info, name, path))
                let w = img.Width / info.Columns
                let h = img.Height / info.Rows

                for row in 0 .. info.Rows - 1 do
                    for col in 0 .. info.Columns - 1 do
                        use tex = new Bitmap(w, h)

                        tex.Mutate<PixelFormats.Rgba32>(fun context ->
                            context.DrawImage(img, Point(-col * w, -row * h), 1.0f) |> ignore
                        )

                        tex.SaveAsPng(Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row col))

                File.Delete(Path.Combine(f, Path.Combine path, sprintf "%s.png" name))
                this.WriteTextureConfig({ info with Mode = Loose }, name, path)
            | Loose -> ()

    member this.StitchTexture(name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->
            let info: TextureConfig = this.GetTextureConfig(name, path)

            match info.Mode with
            | Loose ->
                let img = Option.get (this.LoadLooseTextures(info, name, path))
                img.SaveAsPng(Path.Combine(f, Path.Combine path, sprintf "%s.png" name))

                for row in 0 .. info.Rows - 1 do
                    for col in 0 .. info.Columns - 1 do
                        File.Delete(Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row col))

                this.WriteTextureConfig({ info with Mode = Loose }, name, path)
            | Grid -> ()

    member private this.MutateTexture((col, row), action, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid -> failwith "Not supported for stitched textures"
        | Loose ->

        let img =
            match this.TryReadFile(Array.append path [| sprintf "%s-%i-%i.png" name row col |]) with
            | Some stream ->
                let i = Bitmap.load stream in
                stream.Dispose()
                i
            | None -> failwithf "Couldn't load texture file (%i,%i)" row col

        img.Mutate<PixelFormats.Rgba32>(fun context -> action context |> ignore)
        img.SaveAsPng(Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row col))

    member this.VerticalFlipTexture((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateTexture((col, row), (fun ctx -> ctx.Flip FlipMode.Vertical), name, path)

    member this.HorizontalFlipTexture((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateTexture((col, row), (fun ctx -> ctx.Flip FlipMode.Horizontal), name, path)

    member this.RotateClockwise((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateTexture((col, row), (fun ctx -> ctx.Rotate(RotateMode.Rotate90)), name, path)

    member this.RotateAnticlockwise((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateTexture((col, row), (fun ctx -> ctx.Rotate(RotateMode.Rotate270)), name, path)

    member this.AddTextureRow(src_row: int, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid -> failwith "Not supported for stitched textures"
        | Loose ->

        try
            for col = 0 to info.Columns - 1 do
                File.Copy(
                    Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name src_row col),
                    Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name info.Rows col)
                )

            this.WriteTextureConfig({ info with Rows = info.Rows + 1 }, name, path)
        with err ->
            Logging.Error("Error adding texture row", err)

    member this.AddTextureColumn(src_col: int, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid -> failwith "Not supported for stitched textures"
        | Loose ->

        try
            for row = 0 to info.Rows - 1 do
                File.Copy(
                    Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row src_col),
                    Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row info.Columns)
                )

            this.WriteTextureConfig({ info with Columns = info.Columns + 1 }, name, path)
        with err ->
            Logging.Error("Error adding texture column", err)

    member this.DeleteTextureRow(row: int, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid -> failwith "Not supported for stitched textures"
        | Loose ->

        try
            for col = 0 to info.Columns - 1 do
                File.Delete(Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row col))

                for mrow = row + 1 to info.Rows - 1 do
                    File.Move(
                        Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name mrow col),
                        Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name (mrow - 1) col)
                    )

            this.WriteTextureConfig({ info with Rows = info.Rows - 1 }, name, path)
        with err ->
            Logging.Error("Error removing texture row", err)

    member this.DeleteTextureColumn(col: int, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        let info: TextureConfig = this.GetTextureConfig(name, path)

        match info.Mode with
        | Grid -> failwith "Not supported for stitched textures"
        | Loose ->

        try
            for row = 0 to info.Rows - 1 do
                File.Delete(Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row col))

                for mcol = col + 1 to info.Columns - 1 do
                    File.Move(
                        Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row mcol),
                        Path.Combine(f, Path.Combine path, sprintf "%s-%i-%i.png" name row (mcol - 1))
                    )

            this.WriteTextureConfig({ info with Columns = info.Columns - 1 }, name, path)
        with err ->
            Logging.Error("Error removing texture column", err)

module Storage =

    let NOTESKIN_TEXTURES =
        [|
            "note"
            "noteexplosion"
            "receptor"
            "holdhead"
            "holdbody"
            "holdtail"
            "holdexplosion"
            "receptorlighting"
        |]

    let THEME_TEXTURES = [| "background"; "rain"; "logo"; "cursor" |]

    let THEME_SOUNDS =
        [|
            "hello"
            "click"
            "hover"
            "text-open"
            "text-close"
            "key"
            "notify-error"
            "notify-info"
            "notify-system"
            "notify-task"
        |]
