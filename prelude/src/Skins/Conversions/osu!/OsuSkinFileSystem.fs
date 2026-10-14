namespace Prelude.Skins.Conversions.Osu

open System
open System.IO
open System.IO.Compression

[<AbstractClass>]
type OsuSkinFileSystem() =
    
    abstract member Exists: string -> bool
    abstract member Open: string -> Result<Stream, string>
    abstract member Dispose: unit -> unit
    
    static member NormaliseKey(user_string: string) : string =
        user_string.ToLower().Replace(@"\\", "\\").Replace("\\", "/")
        
    interface IDisposable with
        override this.Dispose() = this.Dispose()

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
        
    override this.Dispose() = ()
        
[<Sealed>]
type OsuSkinZipFileSystem(file: ZipArchive) =
    inherit OsuSkinFileSystem()
    
    let normalise_path(zip_entry_path: string) : string =
         zip_entry_path.ToLower().Replace(@"\\", "/")
    
    let create_entry_map () =
        file.Entries
        |> Seq.filter(fun entry -> entry.Name <> "") // Filters out 'Folder' entries https://stackoverflow.com/questions/40223451/how-to-tell-if-a-ziparchiveentry-is-directory
        |> Seq.map (fun entry -> normalise_path(entry.FullName), entry)
        |> Map.ofSeq
        
    let entry_map =
        let mutable entries = create_entry_map()
        
        let apply_nested_folder_fix() : bool =
            if entries.IsEmpty then false else
            
            let first_path = Map.toSeq entries |> Seq.map fst |> Seq.head
            if first_path.Contains('/') then
                let segment = first_path.Substring(0, first_path.IndexOf('/') + 1)
                let is_common_prefix = entries |> Map.forall (fun path _ -> path.StartsWith(segment)) 
                
                if is_common_prefix then
                    entries <-
                        entries
                        |> Map.toSeq
                        |> Seq.map (fun (path, entry) -> path.Substring(segment.Length), entry)
                        |> Map.ofSeq
                    true
                else
                    false
            else
                false
        
        let mutable depth = 0
        while depth < 2 && apply_nested_folder_fix() do
            depth <- depth + 1
            
        entries
    
    override this.Exists(key: string) : bool =
        entry_map.ContainsKey(key)
        
    override this.Open(key: string) : Result<Stream, string> =
        match entry_map.TryGetValue(key) with
        | true, zip_entry -> Ok(zip_entry.Open())
        | false, _ -> Error(sprintf "Zip entry (out of %i entries) not found: '%s'" entry_map.Count key)
        
    override this.Dispose() = file.Dispose()