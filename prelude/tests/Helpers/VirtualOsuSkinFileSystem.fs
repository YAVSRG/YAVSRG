namespace Prelude.Tests.Helpers

open System.IO
open Prelude.Skins.Conversions.Osu
open SixLabors.ImageSharp
open Prelude

type VirtualOsuSkinFileSystem() =
    inherit OsuSkinFileSystem()

    let mutable virtual_file_map = Map.empty<string, MemoryStream>
    let mutable last_access = ""

    override this.Exists(key: string) : bool = virtual_file_map.ContainsKey(key)

    override this.Open(key: string) : Result<Stream, string> =
        last_access <- key

        match virtual_file_map.TryFind(key) with
        | Some(memory_stream) ->
            memory_stream.Position <- 0
            Ok(memory_stream)
        | None -> Error(sprintf "Virtual file not found: '%s'" key)

    member this.LastAccess() : string =
        let x = last_access
        last_access <- ""
        x

    member this.Add(path: string, data: MemoryStream) : VirtualOsuSkinFileSystem =
        virtual_file_map <- virtual_file_map.Add(OsuSkinFileSystem.NormaliseKey(path), data)
        this

    member this.AddSkinIni(path: string, data: string) : VirtualOsuSkinFileSystem =
        let stream = new MemoryStream()
        let sw = new StreamWriter(stream)
        sw.Write(data)
        this.Add(path, stream)

    member this.AddImage(path: string, image: Bitmap) : VirtualOsuSkinFileSystem =
        let stream = new MemoryStream()
        image.SaveAsPng(stream)
        this.Add(path, stream)
