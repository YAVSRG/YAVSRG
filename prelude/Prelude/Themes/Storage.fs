namespace Prelude.Data.Themes

open System
open System.IO
open System.IO.Compression
open Prelude.Common

(*
    Basic theme I/O stuff. Additional implementation in Interlude for texture-specific things that depend on Interlude
*)

type TextureConfig =
    {
        Columns: int
        Rows: int
        Tiling: bool
    }   
    static member Default =
        {
            Columns = 1
            Rows = 1
            Tiling = true
        }

type StorageType = Zip of ZipArchive * source: string option | Folder of string

type Storage(storage: StorageType) =

    member this.StorageType = storage

    member this.TryReadFile ([<ParamArray>] path: string array) =
        let p = Path.Combine(path)
        try
            match storage with
            | Zip (z, _) -> z.GetEntry(p.Replace(Path.DirectorySeparatorChar, '/')).Open() |> Some
            | Folder f ->
                let p = Path.Combine(f, p)
                File.OpenRead p :> Stream |> Some
        with
        | :? FileNotFoundException | :? DirectoryNotFoundException // File doesnt exist in folder storage
        | :? NullReferenceException -> None // File doesnt exist in zip storage
        | _ -> reraise()
    
    member this.GetFiles ([<ParamArray>] path: string array) =
        let p = Path.Combine(path)
        match storage with
        | Zip (z, _) ->
            let p = p.Replace(Path.DirectorySeparatorChar, '/')
            seq {
                for e in z.Entries do
                    if e.FullName = p + "/" + e.Name && Path.HasExtension e.Name then yield e.Name
            }
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateFiles target |> Seq.map Path.GetFileName

    member this.GetFolders ([<ParamArray>] path: string array) =
        let p = Path.Combine path
        match storage with
        | Zip (z, _) ->
            let p = p.Replace (Path.DirectorySeparatorChar, '/')
            seq {
                for e in z.Entries do
                    if e.Name = "" && e.FullName.Length > p.Length then
                        let s = e.FullName.Substring(p.Length + 1).Split('/')
                        if e.FullName = p + "/" + s.[0] + "/" then yield s.[0]
            }
        | Folder f ->
            let target = Path.Combine(f, p)
            Directory.CreateDirectory target |> ignore
            Directory.EnumerateDirectories target |> Seq.map Path.GetFileName

    /// Gets the specified JSON file, or returns None
    /// None is returned with silent fail if the file did not exist
    /// None is returned with an error logged if something goes wrong while reading the file
    member this.TryGetJson<'T> (writeBack: bool, [<ParamArray>] path: string array) : 'T option =
        let json =
            match this.TryReadFile path with
            | Some stream ->
                use tr = new StreamReader(stream)
                let result =
                    tr.ReadToEnd()
                    |> JSON.FromString<'T>
                    |> 
                        function
                        | Ok v -> Some v
                        | Error err -> 
                            Logging.Error (sprintf "Failed to load %s in user data" (String.concat "/" path), err)
                            None
                stream.Dispose()
                result
            | None -> None
        if writeBack && json.IsSome then this.WriteJson (json.Value, path)
        json

    /// Gets the specified JSON file, or returns the default instance
    /// File is not written back if there was a problem with an existing file
    member this.GetJsonOrDefault<'T> (writeBack: bool, [<ParamArray>] path: string array) : 'T =
        let json, faultInFile =
            match this.TryReadFile path with
            | Some stream ->
                use tr = new StreamReader(stream)
                let result =
                    tr.ReadToEnd()
                    |> JSON.FromString<'T>
                    |> 
                        function
                        | Ok v -> v, false
                        | Error err -> 
                            Logging.Error (sprintf "Error loading %s in user data (Will use default values)" (String.concat "/" path), err)
                            JSON.Default<'T>(), true
                stream.Dispose()
                result
            | None -> JSON.Default<'T>(), false
        if writeBack && not faultInFile then this.WriteJson (json, path)
        json

    member this.WriteJson<'T> (data: 'T, [<ParamArray>] path: string array) =
        match storage with
        | Zip _ -> () // Zip archive is read-only
        | Folder f ->
            let target = Path.Combine (f, Path.Combine path)
            target |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
            JSON.ToFile(target, true) data

    member this.ExtractToFolder targetPath =
        Directory.CreateDirectory targetPath |> ignore
        match storage with
        | Zip (z, _) -> z.ExtractToDirectory targetPath
        | Folder f -> failwith "can only extract zip to folder"
        
    member this.CompressToZip target =
        match storage with
        | Zip (z, _) -> failwith "nyi"
        | Folder f -> ZipFile.CreateFromDirectory(f, target)

module Storage =
    
    let noteskinTextures = [|"note"; "noteexplosion"; "receptor"; "holdhead"; "holdbody"; "holdtail"; "holdexplosion"; "receptorlighting"|]
    let themeTextures = [|"background"; "rain"; "logo"; "cursor"|]
    let rulesetTextures = [|"judgement"; "grade-base"; "grade-lamp-overlay"; "grade-overlay"|]