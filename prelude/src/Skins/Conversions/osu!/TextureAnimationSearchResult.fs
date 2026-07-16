namespace Prelude.Skins.Conversions.Osu

open System.Runtime.CompilerServices
open Percyqaz.Common
open Prelude
            
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
                
    static member private Create(requested_name: string, fallback_name: string, fs: OsuSkinFileSystem) : TextureAnimationSearchResult =
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
            
    [<Extension>]
    static member SearchForAnimation(fs: OsuSkinFileSystem, name: string, fallback: string) : TextureAnimationSearchResult =
        TextureAnimationSearchResult.Create(OsuSkinFileSystem.NormaliseKey(name), OsuSkinFileSystem.NormaliseKey(fallback), fs)
        
    [<Extension>]
    static member SearchForAnimation(fs: OsuSkinFileSystem, name_no_fallback: string) : TextureAnimationSearchResult =
        let normalised = OsuSkinFileSystem.NormaliseKey(name_no_fallback)
        TextureAnimationSearchResult.Create(normalised, normalised, fs)