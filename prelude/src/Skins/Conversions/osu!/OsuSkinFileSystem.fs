namespace Prelude.Skins.Conversions.Osu

open System.IO
open System.IO.Compression

[<AbstractClass>]
type OsuSkinFileSystem() =
    
    abstract member Exists: string -> bool
    abstract member Open: string -> Result<Stream, string>
    
    static member NormaliseKey(user_string: string) : string =
        user_string.ToLower().Replace(@"\\", "\\").Replace("\\", "/")

[<Sealed>]
type OsuSkinFolderFileSystem(path: string) =
    inherit OsuSkinFileSystem()
    
    let path = Path.GetFullPath(path)
    
    let normalise_path(full_file_path: string) : string =
        Path.GetRelativePath(path, full_file_path).ToLower().Replace("\\", "/")
    
    let create_file_map() =
        Directory.GetFiles(path, "*.*", EnumerationOptions(RecurseSubdirectories = true, MaxRecursionDepth = 3))
        |> Seq.map (fun file -> normalise_path(file), file)
        |> Map.ofSeq
        
    let file_map = create_file_map()
    
    override this.Exists(key: string) : bool =
        file_map.ContainsKey(key)
        
    override this.Open(key: string) : Result<Stream, string> =
        match file_map.TryGetValue(key) with
        | true, absolute_path ->
            try Ok(File.OpenRead(absolute_path))
            with exn -> Error(sprintf "File open failed: '%s' -> '%s' (%O) %s" key absolute_path (exn.GetType()) exn.Message)
        | false, _ -> Error(sprintf "File not found: '%s'" key)
        
[<Sealed>]
type OsuSkinZipFileSystem(file: ZipArchive) =
    inherit OsuSkinFileSystem()
    
    let normalise_path(zip_entry_path: string) : string =
         zip_entry_path.ToLower().Replace(@"\\", "/")
    
    let create_entry_map () =
        file.Entries
        |> Seq.map (fun entry -> normalise_path(entry.FullName), entry)
        |> Map.ofSeq
        
    let entry_map = create_entry_map()
    
    override this.Exists(key: string) : bool =
        entry_map.ContainsKey(key)
        
    override this.Open(key: string) : Result<Stream, string> =
        match entry_map.TryGetValue(key) with
        | true, zip_entry -> Ok(zip_entry.Open())
        | false, _ -> Error(sprintf "Zip entry not found: '%s'" key)