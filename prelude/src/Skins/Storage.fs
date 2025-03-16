namespace Prelude.Skins

open System
open System.IO
open System.IO.Compression
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Percyqaz.Data
open Prelude

(*
    Basic theme I/O stuff. Additional implementation in Interlude for texture-specific things that depend on Interlude
*)

[<Json.AutoCodec(false)>]
type TextureConfigLegacy =
    {
        Columns: int
        Rows: int
    }
    static member Default = { Columns = 1; Rows = 1 }

type StorageType =
    | Embedded of ZipArchive
    | Folder of string
    override this.ToString() =
        match this with
        | Embedded _ -> "[Embedded Assets]"
        | Folder f -> Path.GetFileName f + "/"

type TextureRules =
    {
        IsRequired: bool
        MustBeSquare: bool
        MaxGridSize: int * int
    }

type TextureLoadResult =
    | TextureOk of Bitmap * columns: int * rows: int
    | TextureNotRequired
    | TextureError of reason: string

type ValidationSuggestedFix =
    {
        /// Displayed to the user in a confirmation prompt before doing it
        Description: string
        /// Action where after running, the validation message will be resolved
        Action: unit -> unit
    }

type ValidationMessageGuts =
    {
        Element: string
        Message: string
        SuggestedFix: ValidationSuggestedFix option
    }

type ValidationMessage =
    /// This error *needs* to be fixed for the skin to work properly
    | ValidationError of ValidationMessageGuts
    /// This error needs to be fixed for the skin to be uploaded to the repo,
    /// but the noteskin is in some kind of state a user could normally put it in by turning features on and off/editing config
    /// i.e may have a texture they no longer use cause they turned it off, but don't necessarily want to delete it because they are just trying something out
    | ValidationWarning of ValidationMessageGuts

module TextureFileName =
    open System.Text.RegularExpressions

    let private GRID_REGEX = Regex(@"^([a-z\-]+)\[([0-9]+)x([0-9]+)\]\.png$", RegexOptions.IgnoreCase)
    let try_parse_grid (texture_name: string) (file_name: string) : (int * int) option =
        let matches = GRID_REGEX.Matches(file_name)
        if matches.Count > 0 && matches.[0].Groups.[1].ToString().Equals(texture_name, StringComparison.InvariantCultureIgnoreCase) then
            let columns_string = matches.[0].Groups.[2].ToString()
            let rows_string = matches.[0].Groups.[3].ToString()
            match Int32.TryParse(columns_string), Int32.TryParse(rows_string) with
            | (true, c), (true, r) -> Some (c, r)
            | _ -> None
        else None

    let to_grid (texture_id: string) (columns: int, rows: int) : string =
        sprintf "%s[%ix%i].png" texture_id columns rows

    let private LOOSE_REGEX = Regex(@"^([a-z\-]+)-([0-9]+)-([0-9]+)\.png$", RegexOptions.IgnoreCase)
    let try_parse_loose (texture_name: string) (file_name: string) : (int * int) option =
        let matches = LOOSE_REGEX.Matches(file_name)
        if matches.Count > 0 && matches.[0].Groups.[1].ToString().Equals(texture_name, StringComparison.InvariantCultureIgnoreCase) then
            let rows_string = matches.[0].Groups.[2].ToString()
            let columns_string = matches.[0].Groups.[3].ToString()
            match Int32.TryParse(columns_string), Int32.TryParse(rows_string) with
            | (true, c), (true, r) -> Some (c, r)
            | _ -> None
        else None

    let to_loose (texture_id: string) (column: int, row: int) : string =
        sprintf "%s-%i-%i.png" texture_id row column

type Storage(storage: StorageType) =

    member this.Source : StorageType = storage

    member this.IsEmbedded : bool =
        match storage with
        | Embedded _ -> true
        | _ -> false

    abstract member ReloadFromDisk : unit -> unit
    default this.ReloadFromDisk() = ()

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
            Logging.Error
                "IO error reading '%s' in: %O\n  This is unusual, maybe you still have the file open in another program?\n%O"
                (String.concat "/" path)
                storage
                err

            reraise ()
        | _ -> reraise ()

    member this.DeleteFile([<ParamArray>] path: string array) =
        let p = Path.Combine(path)

        match storage with
        | Embedded _ -> ()
        | Folder f ->
            let p = Path.Combine(f, p)

            if File.Exists p then
                File.Delete p

    member this.RenameFile(old_name: string, new_name: string, [<ParamArray>] path: string array) =
        let p = Path.Combine(path)

        match storage with
        | Embedded _ -> ()
        | Folder f ->
            let old_path = Path.Combine(f, p, old_name)
            let new_path = Path.Combine(f, p, new_name)

            if File.Exists old_path && not (File.Exists new_path) then
                File.Move(old_path, new_path)
            else
                Logging.Error "Failed to move file from '%s' to '%s', skin may be messed up as a result" old_path new_path

    /// Returns string names of files in the requested folder
    member this.GetFiles([<ParamArray>] path: string array) : string array =
        let p = Path.Combine(path)

        match storage with
        | Embedded z ->
            let p = p.Replace(Path.DirectorySeparatorChar, '/') |> function "" -> "" | x -> x + "/"

            seq {
                for e in z.Entries do
                    if e.FullName = p + e.Name && Path.HasExtension e.Name then
                        yield e.Name
            }
            |> Array.ofSeq
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateFiles target |> Seq.map Path.GetFileName |> Array.ofSeq

    /// Returns string names of folders in the requested folder
    member this.GetFolders([<ParamArray>] path: string array) : string array =
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
            |> Array.ofSeq
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateDirectories target |> Seq.map Path.GetFileName |> Array.ofSeq

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
                            Logging.Error "Failed to load '%s' in %O: %O" (String.concat "/" path) storage err
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
                            Logging.Error
                                "JSON error reading '%s' in: %O\n  Data was wrong or not formatted properly (Check you aren't missing a comma)\n  Using default values instead\n%O"
                                (String.concat "/" path)
                                storage
                                err

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

    member this.ExtractToFolder (target_directory: string) : bool =
        Directory.CreateDirectory target_directory |> ignore

        match storage with
        | Embedded z ->
            z.ExtractToDirectory target_directory
            true
        | Folder _ ->
            Logging.Error("Can only extract a compressed zip to a folder")
            false

    member this.CompressToZip (target: string) : bool =
        if File.Exists target then
            File.Delete target

        if not (Directory.Exists (Path.GetDirectoryName target)) then
            Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore

        match storage with
        | Embedded _ ->
            Logging.Error("Exporting already zipped content as an archive is not implemented")
            false
        | Folder f ->
            try
                ZipFile.CreateFromDirectory(f, target)
                true
            with
            | :? DirectoryNotFoundException as err ->
                Logging.Error "Couldn't export archive because the folder moved. Did you move it while the game was open?\n%O" err
                false
            | :? IOException as err ->
                Logging.Error "IO exception while exporting archive: %O" err
                false
            | _ -> reraise ()

    // Texture loading

    member private this.LoadGridTexture
        (
            name: string,
            columns: int,
            rows: int,
            path: string array,
            must_be_square: bool
        ) : Result<Bitmap, string> =

        let filename = TextureFileName.to_grid name (columns, rows)
        match this.TryReadFile(Array.append path [| filename |]) with
        | Some stream ->
            match Bitmap.from_stream true stream with
            | None -> Error "This is not a valid image"
            | Some img ->
                if img.Width % columns <> 0 || img.Height % rows <> 0 then
                    Error(
                        sprintf
                            "This texture has mismatched dimensions, should be a multiple of (%i, %i) but is %ix%i"
                            columns
                            rows
                            img.Width
                            img.Height
                    )
                else

                let w = img.Width / columns
                let h = img.Height / rows

                if must_be_square && w <> h then
                    Error(sprintf "This texture needs to be only square images, but currently each image is %ix%i" w h)
                else
                    Ok img
        | None -> Error(sprintf "Couldn't find expected file '%s'" filename)

    member private this.LoadLooseTextures
        (
            name: string,
            columns: int,
            rows: int,
            path: string array,
            must_be_square: bool
        ) : Result<Bitmap, string> =

        let load_img (row: int) (column: int) =
            let filename = TextureFileName.to_loose name (column, row)
            match this.TryReadFile(Array.append path [| filename |]) with
            | Some stream ->
                match Bitmap.from_stream true stream with
                | None -> failwithf "%s is not a valid image" filename
                | Some img -> img
            | None -> failwithf "Couldn't find expected file '%s'" filename

        try

            let base_img = load_img 0 0

            if must_be_square && base_img.Width <> base_img.Height then
                failwithf
                    "This texture needs to be only square images, but %s-0-0 is %ix%i"
                    name
                    base_img.Width
                    base_img.Height

            let atlas =
                new Bitmap(base_img.Width * columns, base_img.Height * rows)

            for row in 0 .. rows - 1 do
                for col in 0 .. columns - 1 do
                    use img = if (row, col) = (0, 0) then base_img else load_img row col

                    if img.Height <> base_img.Height || img.Width <> base_img.Width then
                        failwithf "All images must be the same dimensions, (%i, %i) doesn't match (0, 0)" row col

                    atlas.Mutate<PixelFormats.Rgba32>(fun context ->
                        context.DrawImage(img, Point(col * base_img.Width, row * base_img.Height), 1.0f)
                        |> ignore
                    )

            Ok atlas
        with err ->
            Error err.Message

    member internal this.DetectTextureFormat(name: string, [<ParamArray>] path: string array) : Result<int * int * bool, string> =
        let files = this.GetFiles(path) |> Array.where (fun f -> f.StartsWith(name, StringComparison.InvariantCultureIgnoreCase))
        if files |> Array.exists (fun f -> f.Equals(name + ".png", StringComparison.InvariantCultureIgnoreCase)) then
            let columns, rows, delete_file =
                match this.TryGetJson<TextureConfigLegacy>(false, Array.append path [| name + ".json" |]) with
                | Some conf -> conf.Columns, conf.Rows, true
                | None -> 1, 1, false
            match storage with
            | Folder f ->
                File.Move(
                    Path.Combine(f, Path.Combine path, sprintf "%s.png" name),
                    Path.Combine(f, Path.Combine path, TextureFileName.to_grid name (columns, rows))
                )
                if delete_file then
                    File.Delete(
                        Path.Combine(f, Path.Combine path, sprintf "%s.json" name)
                    )
                Ok(columns, rows, true)
            | Embedded _ -> Error "Legacy texture detected in embedded assets"
        else

        match
            files
            |> Seq.choose (TextureFileName.try_parse_grid name)
            |> List.ofSeq
        with
        | x :: y :: _ as xs -> Error (sprintf "Multiple textures for '%s': %s" name (xs |> Seq.map (TextureFileName.to_grid name) |> String.concat ", "))
        | (columns, rows) :: [] -> Ok(columns, rows, true)
        | [] ->

        let loose =
            files
            |> Seq.choose (TextureFileName.try_parse_loose name)
            |> Array.ofSeq
        if loose.Length = 0 then
            Error(sprintf "No files detected for texture '%s', grid or loose" name)
        else
            this.DeleteFile(Path.Combine(Array.append path [| sprintf "%s.json" name |] ))
            Ok((Array.maxBy fst loose |> fst) + 1, (Array.maxBy snd loose |> snd) + 1, false)

    member internal this.LoadTexture
        (
            name: string,
            rules: TextureRules,
            [<ParamArray>] path: string array
        ) : TextureLoadResult =

        match this.DetectTextureFormat(name, path) with
        | Error reason -> TextureError reason
        | Ok (columns, rows, is_grid) ->

        match
            let max_rows, max_columns = rules.MaxGridSize in

            if columns < 1 then
                Error "Columns must be a positive number"
            elif columns > max_columns then
                Error(sprintf "Columns must be at most %i for this texture" max_columns)
            elif rows < 1 then
                Error "Rows must be a positive number"
            elif rows > max_rows then
                Error(sprintf "Rows must be at most %i for this texture" max_rows)
            else

            if is_grid then
                this.LoadGridTexture(name, columns, rows, path, rules.MustBeSquare)
            else
                this.LoadLooseTextures(name, columns, rows, path, rules.MustBeSquare)
        with
        | Ok img -> TextureOk(img, columns, rows)
        | Error reason ->
            if rules.IsRequired then
                TextureError reason
            else
                TextureNotRequired

    member private this.ValidateGridTexture
        (
            name: string,
            columns: int,
            rows: int,
            rules: TextureRules,
            [<ParamArray>] path: string array
        ) : ValidationMessage seq =

        seq {
            let filename = TextureFileName.to_grid name (columns, rows)
            match this.TryReadFile(Array.append path [| filename |]) with
            | Some stream ->
                match Bitmap.from_stream true stream with
                | None ->
                    yield
                        ValidationError
                            {
                                Element = name
                                Message = sprintf "'%s' is not a valid image" filename
                                SuggestedFix = None
                            }
                | Some img ->
                    if img.Width % columns <> 0 || img.Height % rows <> 0 then
                        let could_rows_columns_swap =
                            img.Width % rows = 0 && img.Height % columns = 0

                        if could_rows_columns_swap then
                            yield
                                ValidationError
                                    {
                                        Element = name
                                        Message =
                                            sprintf
                                                "'%s' dimensions should be multiple of %ix%i but are %ix%i\nPerhaps you've got rows and columns the wrong way around?"
                                                filename
                                                columns
                                                rows
                                                img.Width
                                                img.Height
                                        SuggestedFix =
                                            Some
                                                {
                                                    Description = "Swap rows and columns"
                                                    Action =
                                                        fun () ->
                                                            this.RenameFile(
                                                                (TextureFileName.to_grid name (columns, rows)),
                                                                (TextureFileName.to_grid name (rows, columns)),
                                                                path
                                                            )
                                                }
                                    }
                        else
                            yield
                                ValidationError
                                    {
                                        Element = name
                                        Message =
                                            sprintf
                                                "'%s' dimensions should be multiple of %ix%i but are %ix%i"
                                                filename
                                                columns
                                                rows
                                                img.Width
                                                img.Height
                                        SuggestedFix = None
                                    }
                    else
                        let w = img.Width / columns
                        let h = img.Height / rows

                        if rules.MustBeSquare && w <> h then

                            let could_rows_columns_swap = img.Width / rows = img.Height / columns
                            if could_rows_columns_swap then
                                yield
                                    ValidationError
                                        {
                                            Element = name
                                            Message =
                                                sprintf
                                                    "'%s' must be square images, but each image is %ix%i\nPerhaps you've got rows and columns the wrong way around?"
                                                    filename
                                                    w
                                                    h
                                            SuggestedFix =
                                                Some
                                                    {
                                                        Description = "Swap rows and columns"
                                                        Action =
                                                            fun () ->
                                                                this.RenameFile(
                                                                    (TextureFileName.to_grid name (columns, rows)),
                                                                    (TextureFileName.to_grid name (rows, columns)),
                                                                    path
                                                                )
                                                    }
                                        }
                            else
                                yield
                                    ValidationError
                                        {
                                            Element = name
                                            Message =
                                                sprintf
                                                    "'%s' must be square images, but each image is %ix%i"
                                                    filename
                                                    w
                                                    h
                                            SuggestedFix = None
                                        }

                    if not rules.IsRequired then
                        yield
                            ValidationWarning
                                {
                                    Element = name
                                    Message = sprintf "%s is not used" name
                                    SuggestedFix =
                                        Some
                                            {
                                                Description = sprintf "Delete '%s'" filename
                                                Action =
                                                    fun () ->
                                                        this.DeleteFile(Array.append path [| filename |])
                                            }
                                }
            | None ->
                if rules.IsRequired then
                    yield
                        ValidationError
                            {
                                Element = name
                                Message = sprintf "'%s' is missing" filename
                                SuggestedFix = None
                            }
        }

    member private this.ValidateLooseTextures
        (
            name: string,
            columns: int,
            rows: int,
            rules: TextureRules,
            [<ParamArray>] path: string array
        ) : ValidationMessage seq =
        seq {
            let base_filename = TextureFileName.to_loose name (0, 0)
            let check_img (width, height) row column =
                seq {
                    let filename = TextureFileName.to_loose name (column, row)
                    match this.TryReadFile(Array.append path [| filename |]) with
                    | Some stream ->
                        match Bitmap.from_stream true stream with
                        | None ->
                            yield
                                ValidationError
                                    {
                                        Element = name
                                        Message = sprintf "'%s' is not a valid image" filename
                                        SuggestedFix = None
                                    }
                        | Some img ->
                            if (img.Width, img.Height) <> (width, height) then
                                yield
                                    ValidationError
                                        {
                                            Element = name
                                            Message =
                                                sprintf
                                                    "All images must be the same dimensions, '%s' doesn't match '%s'" filename base_filename
                                            SuggestedFix = None
                                        }
                    | None ->
                        yield
                            ValidationError
                                {
                                    Element = name
                                    Message =
                                        sprintf
                                            "'%s' is missing"
                                            filename
                                    SuggestedFix = None
                                }
                }

            let base_filename = TextureFileName.to_loose name (0, 0)
            match this.TryReadFile(Array.append path [| base_filename |]) with
            | Some stream ->
                match Bitmap.from_stream true stream with
                | None ->
                    yield
                        ValidationError
                            {
                                Element = name
                                Message = sprintf "'%s' is not a valid image" base_filename
                                SuggestedFix = None
                            }
                | Some base_img ->
                    if rules.MustBeSquare && base_img.Width <> base_img.Height then
                        yield
                            ValidationError
                                {
                                    Element = name
                                    Message =
                                        sprintf
                                            "This texture must be square images, but '%s' is %ix%i"
                                            base_filename
                                            base_img.Width
                                            base_img.Height
                                    SuggestedFix = None
                                }

                    for row in 0 .. rows - 1 do
                        for column in 0 .. columns - 1 do
                            if (row, column) <> (0, 0) then
                                yield! check_img (base_img.Width, base_img.Height) row column

                    if not rules.IsRequired then
                        yield
                            ValidationWarning
                                {
                                    Element = name
                                    Message = sprintf "'%s' is not used" name
                                    SuggestedFix =
                                        Some
                                            {
                                                Description = sprintf "Delete all '%s' images" name
                                                Action =
                                                    fun () ->
                                                        for file in this.GetFiles path do
                                                            match TextureFileName.try_parse_loose name file with
                                                            | None -> ()
                                                            | Some _ ->
                                                                this.DeleteFile(Array.append path [| file |])
                                            }
                                }
            | None ->
                if rules.IsRequired then
                    yield
                        ValidationError
                            {
                                Element = name
                                Message =
                                    sprintf
                                        "'%s' is missing"
                                        base_filename
                                SuggestedFix = None
                            }
        }

    member internal this.ValidateTexture
        (
            name: string,
            rules: TextureRules,
            [<ParamArray>] path: string array
        ) : ValidationMessage seq =

        seq {
            match this.DetectTextureFormat(name, path) with
            | Error reason -> if rules.IsRequired then yield ValidationError { Element = name; Message = reason; SuggestedFix = None }
            | Ok (columns, rows, is_grid) ->

            // Validate rows, columns
            let max_rows, max_columns = rules.MaxGridSize
            if columns < 1 || columns > max_columns then
                yield
                    ValidationError
                        {
                            Element = name
                            Message = sprintf "Columns must be between %i and %i" 1 max_columns
                            SuggestedFix = None
                        }
            elif rows < 1 || rows > max_rows then
                yield
                    ValidationError
                        {
                            Element = name
                            Message = sprintf "Rows must be between %i and %i" 1 max_rows
                            SuggestedFix = None
                        }
            else

            if is_grid then
                yield! this.ValidateGridTexture(name, columns, rows, rules, path)
            else
                yield! this.ValidateLooseTextures(name, columns, rows, rules, path)
        }

    member this.TextureIsGrid(name: string) : bool =
        match this.DetectTextureFormat(name) with
        | Ok (_, _, is_grid) -> is_grid
        | _ -> false

    member this.SplitTexture(name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

            match this.DetectTextureFormat(name, path) with
            | Error reason -> Logging.Error "Couldn't split texture '%s': %s" name reason
            | Ok (columns, rows, is_grid) ->

            if is_grid then
                match this.LoadGridTexture(name, columns, rows, path, false) with
                | Error _ -> Logging.Error "Couldn't split texture '%s' because it couldn't be loaded" name
                | Ok img ->

                let w = img.Width / columns
                let h = img.Height / rows

                for row in 0 .. rows - 1 do
                    for col in 0 .. columns - 1 do
                        use tex = new Bitmap(w, h)

                        tex.Mutate<PixelFormats.Rgba32>(fun context ->
                            context.DrawImage(img, Point(-col * w, -row * h), 1.0f) |> ignore
                        )

                        tex.SaveAsPng(Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, row)))

                File.Delete(Path.Combine(f, Path.Combine path, TextureFileName.to_grid name (columns, rows)))

    member this.StitchTexture(name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

            match this.DetectTextureFormat(name, path) with
            | Error reason -> Logging.Error "Couldn't stitch texture '%s': %s" name reason
            | Ok (columns, rows, is_grid) ->

            if not is_grid then
                match this.LoadLooseTextures(name, columns, rows, path, false) with
                | Error _ -> Logging.Error "Couldn't stitch texture '%s' because it couldn't be loaded" name
                | Ok img ->

                img.SaveAsPng(Path.Combine(f, Path.Combine path, TextureFileName.to_grid name (columns, rows)))

                for row in 0 .. rows - 1 do
                    for col in 0 .. columns - 1 do
                        File.Delete(Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, row)))

    member private this.MutateLooseTexture((col, row), action: (IImageProcessingContext -> IImageProcessingContext), name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason ->
            Logging.Error "Cannot mutate '%s': %s" name reason
            false
        | Ok (columns, rows, is_grid) ->

        if is_grid then
            Logging.Warn "Cannot mutate %s (%i, %i) as it is a grid texture" name col row
            false
        elif col < 0 || col >= columns  || row < 0 || row >= rows then
            false
        else

        let filename = TextureFileName.to_loose name (col, row)
        match this.TryReadFile(Array.append path [| filename |]) with
        | Some stream ->
            match Bitmap.from_stream true stream with
            | None ->
                Logging.Warn "'%s' is not a valid image" filename
                false
            | Some img ->
                img.Mutate<PixelFormats.Rgba32>(fun context -> action context |> ignore)
                img.SaveAsPng(Path.Combine(f, Path.Combine path, filename))
                true
        | None ->
            Logging.Warn "Couldn't find file '%s'" filename
            false

    member this.MutateLooseTextures(action: (IImageProcessingContext -> IImageProcessingContext), name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason ->
            Logging.Error "Cannot mutate '%s': %s" name reason
            false
        | Ok (columns, rows, is_grid) ->

        if is_grid then
            Logging.Warn "Cannot mutate %s as it is a grid texture" name
            false
        else

        for col = 0 to columns - 1 do
            for row = 0 to rows - 1 do
                let filename = TextureFileName.to_loose name (col, row)
                match this.TryReadFile(Array.append path [| filename |]) with
                | Some stream ->
                    match Bitmap.from_stream true stream with
                    | None ->
                        Logging.Warn "'%s' is not a valid image" filename
                    | Some img ->
                        img.Mutate<PixelFormats.Rgba32>(fun context -> action context |> ignore)
                        img.SaveAsPng(Path.Combine(f, Path.Combine path, filename))
                | None ->
                    Logging.Warn "Couldn't find file '%s'" filename
        true

    member this.VerticalFlipTexture((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateLooseTexture((col, row), (fun (ctx: IImageProcessingContext) -> ctx.Flip FlipMode.Vertical), name, path)

    member this.HorizontalFlipTexture((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateLooseTexture((col, row), (fun (ctx: IImageProcessingContext) -> ctx.Flip FlipMode.Horizontal), name, path)

    member this.RotateClockwise((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateLooseTexture((col, row), (fun (ctx: IImageProcessingContext) -> ctx.Rotate(RotateMode.Rotate90)), name, path)

    member this.RotateAnticlockwise((col, row), name: string, [<ParamArray>] path: string array) =
        this.MutateLooseTexture((col, row), (fun (ctx: IImageProcessingContext) -> ctx.Rotate(RotateMode.Rotate270)), name, path)

    member this.CycleTextures(positions: (int * int) array, name: string, [<ParamArray>] path: string array) =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason ->
            Logging.Error "Cannot mutate '%s': %s" name reason
            false
        | Ok (columns, rows, is_grid) ->

        if is_grid then
            Logging.Warn "Cannot mutate %s as it is a grid texture" name
            false
        elif positions.Length < 2 || positions |> Array.exists (fun (col, row) -> col < 0 || col >= columns  || row < 0 || row >= rows) then
            false
        else

        let EXTRA_FILE = sprintf "%s.png.old" name
        let mutable last_file = EXTRA_FILE
        for (col, row) in positions do
            let filename = TextureFileName.to_loose name (col, row)
            this.RenameFile(filename, last_file, path)
            last_file <- filename
        this.RenameFile(EXTRA_FILE, last_file, path)
        true

    member this.AddLooseTextureRow(src_row: int, name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason -> Logging.Error "Couldn't edit texture '%s': %s" name reason; false
        | Ok (columns, rows, is_grid) ->

        if is_grid then
            Logging.Warn "Cannot clone %s (*, %i) as it is a grid texture" name src_row
            false
        else

        try
            for col = 0 to columns - 1 do
                File.Copy(
                    Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, src_row)),
                    Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, rows))
                )
            true
        with err ->
            Logging.Error "Error adding texture row: %O" err
            false

    member this.AddLooseTextureColumn(src_col: int, name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason -> Logging.Error "Couldn't edit texture '%s': %s" name reason; false
        | Ok (columns, rows, is_grid) ->

        if is_grid then
            Logging.Warn "Cannot clone %s (%i, *) as it is a grid texture" name src_col
            false
        else

        try
            for row = 0 to rows - 1 do
                File.Copy(
                    Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (src_col, row)),
                    Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (columns, row))
                )
            true
        with err ->
            Logging.Error "Error adding texture column: %O" err
            false

    member this.DeleteLooseTextureRow(row: int, name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason -> Logging.Error "Couldn't edit texture '%s': %s" name reason; false
        | Ok (columns, rows, is_grid) ->

        if rows <= 1 then false else

        if is_grid then
            Logging.Warn "Cannot delete %s (*, %i) as it is a grid texture" name row
            false
        else

        try
            for col = 0 to columns - 1 do
                File.Delete(Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, row)))

                for mrow = row + 1 to rows - 1 do
                    File.Move(
                        Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, mrow)),
                        Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, mrow - 1))
                    )
            true
        with err ->
            Logging.Error "Error removing texture row: %O" err
            false

    member this.DeleteLooseTextureColumn(col: int, name: string, [<ParamArray>] path: string array) : bool =
        match storage with
        | Embedded _ -> failwith "Not supported for zipped content"
        | Folder f ->

        match this.DetectTextureFormat(name, path) with
        | Error reason -> Logging.Error "Couldn't edit texture '%s': %s" name reason; false
        | Ok (columns, rows, is_grid) ->

        if columns <= 1 then false else

        if is_grid then
            Logging.Warn "Cannot delete %s (%i, *) as it is a grid texture" name col
            false
        else

        try
            for row = 0 to rows - 1 do
                File.Delete(Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (col, row)))

                for mcol = col + 1 to columns - 1 do
                    File.Move(
                        Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (mcol, row)),
                        Path.Combine(f, Path.Combine path, TextureFileName.to_loose name (mcol - 1, row))
                    )
            true
        with err ->
            Logging.Error "Error removing texture column: %O" err
            false

    interface IDisposable with
        member this.Dispose() =
            match this.Source with
            | Embedded archive -> archive.Dispose()
            | Folder _ -> ()