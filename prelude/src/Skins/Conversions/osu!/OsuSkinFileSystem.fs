namespace Prelude.Skins.Conversions.Osu

open System.IO

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