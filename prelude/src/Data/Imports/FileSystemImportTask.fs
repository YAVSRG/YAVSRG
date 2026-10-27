namespace Prelude.Data.Imports

open System.IO
open System.IO.Compression
open Prelude.Formats.Conversions
open Prelude.Data.Imports
open Prelude.Data.Library

type FileSystemImportTask(path: string, library: Library) =
    inherit BackgroundTask<ConversionResult option>()

    override this.Run(ctx: ITaskContext) : Async<ConversionResult option> =
        async {
            match File.GetAttributes(path) &&& FileAttributes.Directory |> int with
            | 0 ->
                match path with
                | ChartFile ext ->
                    let folder_name =
                        match ext with
                        | ".osu" -> "osu!"
                        | ".qua" -> "Quaver"
                        | _ -> "Singles"

                    let! result =
                        FileSystemImports.convert_song_folder(
                            Path.GetDirectoryName(path),
                            ConversionOptions.Pack(folder_name, None, Copy),
                            library
                        )

                    result.LogInfo()
                    return Some result
                | ChartArchive _ ->
                    let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                    if Directory.Exists(dir) && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not then
                        return Some(ConversionResult.Skip(path, "Target extraction folder already exists"))
                    else
                        ZipFile.ExtractToDirectory(path, dir)
                        let! result = FileSystemImportTask(dir, library).Run(ctx)
                        FileSystemServices.delete_folder.Request(dir, ignore)
                        return result
                | _ -> return None
            | _ ->
                match path with
                | SongFolder ext ->
                    let folder_name =
                        match ext with
                        | ".osu" -> "osu!"
                        | ".qua" -> "Quaver"
                        | _ -> "Singles"

                    let! result =
                        FileSystemImports.convert_song_folder(
                            path,
                            ConversionOptions.Pack(folder_name, None, Copy),
                            library
                        )

                    result.LogInfo()
                    return Some result
                | FolderOfOszs ->
                    let! result = FileSystemImports.convert_folder_of_oszs(path, library, ctx)
                    result.LogInfo()
                    return Some result
                | PackFolder ->
                    let folder_name =
                        match Path.GetFileName(path) with
                        | "Songs" ->
                            if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!"
                            elif path |> Path.GetDirectoryName |> Path.GetFileName = "Quaver" then "Quaver"
                            else "Songs"
                        | s -> s

                    let! result =
                        FileSystemImports.convert_pack_folder(
                            path,
                            ConversionOptions.Pack(folder_name, None, Copy),
                            library,
                            ctx
                        )

                    result.LogInfo()
                    return Some result
                | FolderOfPacks ->
                    let mutable results = ConversionResult.Empty
                    let pack_folders = Directory.GetDirectories(path)

                    for i, pack_folder in Array.indexed pack_folders do
                        let pack_name = Path.GetFileName(pack_folder)

                        let! result =
                            FileSystemImports.convert_pack_folder(
                                pack_folder,
                                ConversionOptions.Pack(pack_name, None, Copy),
                                library,
                                ctx.Subtask(pack_name, i + 1, pack_folders.Length)
                            )

                        results <- results.CombineWith(result)

                    results.LogInfo()
                    return Some results
                | _ -> return None
        }
