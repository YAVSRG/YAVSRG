namespace YAVSRG.CLI

open System.IO

module SourceFiles =

    let rec walk_fs_files (directory: string) : string seq =
        seq {
            for file in Directory.GetFiles(directory) do
                if Path.GetExtension(file).ToLower() = ".fs" then
                    yield file

            for subdirectory in Directory.GetDirectories(directory) do
                let subdirectory_name = Path.GetFileName subdirectory

                if subdirectory_name <> "bin" && subdirectory_name <> "obj" then
                    yield! walk_fs_files subdirectory
        }

    let walk_fs_file_contents (directory: string) : (string * string) seq =
        walk_fs_files(directory)
        |> Seq.map (fun file_path -> file_path, File.ReadAllText(file_path))