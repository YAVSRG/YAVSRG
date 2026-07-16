namespace Prelude.Skins.Conversions.Osu

open System.Runtime.CompilerServices
open Percyqaz.Common
open Prelude
        
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
            
    static member private Create(requested_name: string, fallback_name: string, fs: OsuSkinFileSystem) : TextureSearchResult =
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
            
    [<Extension>]
    static member SearchForTexture(fs: OsuSkinFileSystem, name: string, fallback: string) : TextureSearchResult =
        TextureSearchResult.Create(OsuSkinFileSystem.NormaliseKey(name), OsuSkinFileSystem.NormaliseKey(fallback), fs)
        
    [<Extension>]
    static member SearchForTexture(fs: OsuSkinFileSystem, name_no_fallback: string) : TextureSearchResult =
        let normalised = OsuSkinFileSystem.NormaliseKey(name_no_fallback)
        TextureSearchResult.Create(normalised, normalised, fs)