namespace Prelude.Data.Library

open System
open System.IO
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type AssetLocation =
    | Absolute of string
    | Hash of string
    | Missing
    // todo: remove this hard-coded path logic and go through a member on AssetStorage of ChartDatabase instead
    member this.Path : string option =
        match this with
        | Absolute p -> Some p
        | Hash h ->
            Path.Combine(get_game_folder "Songs", ".assets", h.Substring(0, 2), h) |> Some
        | Missing -> None

[<AbstractClass>]
type AssetStorage() =
    abstract member Add: string -> string
    abstract member Remove: string -> int64
    abstract member Contains: string -> bool
    abstract member GetPath: string -> string
    abstract member Enumerate: unit -> string seq

type FileSystemAssetStorage(assets_path: string) =
    inherit AssetStorage()

    override this.Add(file_path: string) : string =

        let inline compute_asset_hash (file_path: string) : string =
            use stream = File.OpenRead(file_path)
            SHA256.HashData(stream) |> BitConverter.ToString |> _.Replace("-", "")

        let hash = compute_asset_hash(file_path)

        let target_folder = Path.Combine(assets_path, hash.Substring(0, 2))
        let target_path = Path.Combine(target_folder, hash)

        Directory.CreateDirectory(target_folder) |> ignore

        if not(File.Exists(target_path)) then
            File.Copy(file_path, target_path)

        hash

    override this.Remove(hash: string) : int64 =
        let path = this.GetPath(hash)

        try
            let info = FileInfo(path)
            let length = info.Length
            info.Delete()
            length
        with err ->
            Logging.Warn "Error deleting file %s: %O" path err
            -1L

    override this.Contains(hash: string) : bool = File.Exists(this.GetPath(hash))

    override this.GetPath(hash: string) : string =
        Path.Combine(assets_path, hash.Substring(0, 2), hash)

    override this.Enumerate() : string seq =
        seq {
            for directory in Directory.EnumerateDirectories(assets_path) do
                for file in Directory.EnumerateFiles(directory) do
                    let hash_file_name = Path.GetFileName(file)
                    yield hash_file_name
        }