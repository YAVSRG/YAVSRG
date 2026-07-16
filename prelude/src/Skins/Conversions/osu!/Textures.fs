namespace Prelude.Skins.Conversions.Osu

open System.IO
open Percyqaz.Common
open SixLabors.ImageSharp.Processing
open Prelude

type OsuSkinFileSystem(path: string) =
    let normalise_path(full_file_path: string) : string =
        Path.GetRelativePath(path, full_file_path).ToLower().Replace("\\", "/")
    
    let create_file_map() =
        Directory.GetFiles(path, "*.*", EnumerationOptions(RecurseSubdirectories = true, MaxRecursionDepth = 3))
        |> Seq.map (fun file -> normalise_path(file), file)
        |> Map.ofSeq
        
    let file_map = create_file_map()
    
    member this.Exists(key: string) : bool =
        file_map.ContainsKey(key)
        
    member this.Open(key: string) : Result<Stream, string> =
        match file_map.TryGetValue(key) with
        | true, absolute_path ->
            try Ok(File.OpenRead(absolute_path))
            with exn -> Error(sprintf "File open failed: '%s' -> '%s' (%O) %s" key absolute_path (exn.GetType()) exn.Message)
        | false, _ -> Error(sprintf "File not found: '%s'" key)
        
    static member NormaliseKey(user_string: string) : string =
        user_string.ToLower().Replace(@"\\", "\\").Replace("\\", "/")

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
        
    static member TransparentFallback() : LoadedTexture =
        { Image = new Bitmap(64, 64); Is2x = true }
        
[<RequireQualifiedAccess>]
type TextureSearchResult =
    | Ok of key: string
    | Error of tried_files: string list
        
    member this.ToOption() : _ option =
        match this with
        | Ok key -> Some key
        | Error _ -> None
    
    static member private TryFile(key: string, fs: OsuSkinFileSystem) : TextureSearchResult =
        if fs.Exists(key) then Ok key else Error [key]
        
    static member private TryImage2xOr1x(key: string, fs: OsuSkinFileSystem) : TextureSearchResult =
        TextureSearchResult
            .TryFile(key + "@2x.png", fs)
            .ThenTryFile(key + ".png", fs)
        
    member private this.ThenTryFile(key: string, fs: OsuSkinFileSystem) : TextureSearchResult =
        match this with
        | Ok _ -> this
        | Error tried_files ->
            if fs.Exists(key) then Ok key else Error (key :: tried_files)
            
    member private this.ThenTryImage2xOr1x(key: string, fs: OsuSkinFileSystem) : TextureSearchResult =
        this
            .ThenTryFile(key + "@2x.png", fs)
            .ThenTryFile(key + ".png", fs)
            
    static member Create(requested_name: string, fallback_name: string, fs: OsuSkinFileSystem) : TextureSearchResult =
        let requested_name = OsuSkinFileSystem.NormaliseKey(requested_name)
        if requested_name = fallback_name then
            TextureSearchResult
                .TryImage2xOr1x(fallback_name, fs)
        else
            TextureSearchResult
                .TryImage2xOr1x(requested_name, fs)
                .ThenTryImage2xOr1x(fallback_name, fs)
            
    member this.ThrowIfNotFound() : TextureSearchResult =
        match this with
        | Ok _ -> this
        | Error tried_files ->
            failwithf "No matching files found! Checked for: \n %s\nCannot continue without this" (tried_files |> List.rev |> Seq.map (sprintf "'%s'") |> String.concat "\n ")
            
    member this.Load(fs: OsuSkinFileSystem) : LoadedTexture =
        match this with
        | Ok key ->
            match fs.Open(key) with
            | Result.Ok image_stream ->
                
                match Bitmap.from_stream true image_stream with
                | Some bitmap ->
                    { Image = bitmap; Is2x = key.EndsWith("@2x.png") }
                | None ->
                    Logging.Debug "Texture error, using fallback: File stream ('%s') OK but image data invalid or corrupt" key
                    LoadedTexture.TransparentFallback()
                    
            | Result.Error file_error ->
                Logging.Debug "Texture error, using fallback: %s" file_error
                LoadedTexture.TransparentFallback()
        | Error tried_files ->
            Logging.Debug "Texture error, using fallback: No matching files found! Checked for: \n %s" (tried_files |> List.rev |> Seq.map (sprintf "'%s'") |> String.concat "\n ")
            LoadedTexture.TransparentFallback()
            
[<RequireQualifiedAccess>]
type TextureAnimationSearchResult =
    | Ok of frames: string list
    | Error of tried_files: string list
    
    member this.FirstFrame : TextureSearchResult =
        match this with
        | Ok frames -> TextureSearchResult.Ok (List.head frames)
        | Error tried_files -> TextureSearchResult.Error tried_files
        
    member this.ToOption() : _ option =
        match this with
        | Ok frames -> Some frames
        | Error _ -> None
    
    static member private TryFrames2xOr1x(key: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
        let mutable found: string list = []
        let mutable i = 0
        
        let inline find_next() =
            let frame_2x = key + "-" + i.ToString() + "@2x.png"
            let frame_1x = key + "-" + i.ToString() + ".png"
            
            if fs.Exists(frame_2x) then
                found <- frame_2x :: found
                true
            elif fs.Exists(frame_1x) then
                found <- frame_1x :: found
                true
            else
                false
                
        while find_next() do
            i <- i + 1
            
        if found <> [] then
            Ok(List.rev found)
        else
            Error([key + "-0.png"; key + "-0@2x.png"])
        
    member private this.ThenTryFile(key: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
        match this with
        | Ok _ -> this
        | Error tried_files ->
            if fs.Exists(key) then Ok [key] else Error (key :: tried_files)
            
    member private this.ThenTryImage2xOr1x(key: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
        this
            .ThenTryFile(key + "@2x.png", fs)
            .ThenTryFile(key + ".png", fs)
            
    member private this.ThenTryFrames2xOr1x(key: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
        match this with
        | Ok _ -> this
        | Error tried_files ->
            match TextureAnimationSearchResult.TryFrames2xOr1x(key, fs) with
            | Ok files -> Ok files
            | Error more_tried_files -> Error (more_tried_files @ tried_files)
                
    static member Create(requested_name: string, fallback_name: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
        let requested_name = OsuSkinFileSystem.NormaliseKey(requested_name)
        if requested_name = fallback_name then
            TextureAnimationSearchResult
                .TryFrames2xOr1x(fallback_name, fs)
                .ThenTryImage2xOr1x(fallback_name, fs)
        else
            TextureAnimationSearchResult
                .TryFrames2xOr1x(requested_name, fs)
                .ThenTryImage2xOr1x(requested_name, fs)
                .ThenTryFrames2xOr1x(fallback_name, fs)
                .ThenTryImage2xOr1x(fallback_name, fs)
            
    member this.ThrowIfNotFound() : TextureAnimationSearchResult =
        match this with
        | Ok _ -> this
        | Error tried_files ->
            failwithf "No matching files found! Checked for: \n %s\nCannot continue without this" (tried_files |> List.rev |> Seq.map (sprintf "'%s'") |> String.concat "\n ")
            
    member private this.TryLoad(fs: OsuSkinFileSystem) : LoadedTexture list option =
        
        let try_load_key(key: string) : LoadedTexture option =
            match fs.Open(key) with
                | Result.Ok image_stream ->
                    match Bitmap.from_stream true image_stream with
                    | Some bitmap ->
                        Some { Image = bitmap; Is2x = key.EndsWith("@2x.png") }
                    | None ->
                        Logging.Debug "Animation frame error: File stream ('%s') OK but image data invalid or corrupt" key
                        None
                | Result.Error file_error ->
                    Logging.Debug "Animation frame error, using fallback: %s" file_error
                    None
                    
        let replace_missing_frame (successful_frame: LoadedTexture) (i: int) =
            function
            | None ->
                Logging.Debug "Frame %i did not load, using another as a replacement" i
                successful_frame
            | Some frame -> frame
                    
        match this with
        | Ok frames ->
            let results = frames |> List.map try_load_key
            let first_success = List.tryPick id results
            
            match first_success with
            | Some first_successful_frame ->
                Some(List.mapi (replace_missing_frame first_successful_frame) results)
            | None ->
                Logging.Debug "Texture/animation error, using fallback: 0 valid frames were loaded"
                None
                
        | Error tried_files ->
            Logging.Debug "Texture/animation error, using fallback: No matching files found! Checked for: \n %s" (tried_files |> List.rev |> Seq.map (sprintf "'%s'") |> String.concat "\n ")
            None
            
    member this.Load(fs: OsuSkinFileSystem) : LoadedTexture list =
        match this.TryLoad(fs) with
        | Some textures -> textures
        | None -> [ LoadedTexture.TransparentFallback() ]
            
    static member LoadMany(animations: TextureAnimationSearchResult list, fs: OsuSkinFileSystem) : LoadedTexture list list =
        
        let try_load_animation (i: int) (animation: TextureAnimationSearchResult) : LoadedTexture list option =
            Logging.Debug "Loading animated texture %i" i
            animation.TryLoad(fs)
                    
        let replace_missing_animation (successful_animation: LoadedTexture list) (i: int) =
            function
            | None ->
                Logging.Debug "Animation %i did not load, using another as a replacement" i
                successful_animation
            | Some frame -> frame
                    
        let results = animations |> List.mapi try_load_animation
        let first_success = List.tryPick id results
        
        match first_success with
        | Some first_successful_animation ->
            List.mapi (replace_missing_animation first_successful_animation) results
        | None ->
            Logging.Debug "Multi-animation error, using fallback: 0 valid animations were loaded"
            [ [ LoadedTexture.TransparentFallback() ] ]