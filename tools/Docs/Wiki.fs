namespace YAVSRG.CLI.Features

open System.IO
open Prelude.Common
open Percyqaz.Data
open YAVSRG.CLI.Utils

module Wiki =

    //// WIKI GENERATOR

    [<Json.AutoCodec>]
    type WikiPage =
        {
            Title: string
            Folder: string
            Filename: string
        }

    let parse_wiki_file (path: string) =
        let text = File.ReadAllText(path).Replace("\r", "")
        let split = text.Split("---", 3, System.StringSplitOptions.TrimEntries)

        if split.Length <> 3 || split.[0] <> "" then
            failwithf "Problem with format of wiki file: %s" path

        let header_info =
            try
                let header =
                    split.[1].Split("\n")
                    |> Array.map (fun line ->
                        let parts = line.Split(":", System.StringSplitOptions.TrimEntries) in (parts.[0], parts.[1])
                    )
                    |> Map.ofSeq

                if not (header.ContainsKey "title") then
                    failwith "Page is missing 'title'"

                if not (header.ContainsKey "folder") then
                    failwith "Page is missing 'folder'"

                header
            with err ->
                failwithf "Problem parsing header of file: %s (%O)" path err

        {
            Title = header_info.["title"]
            Folder = header_info.["folder"]
            Filename = Path.GetFileNameWithoutExtension(path)
        }

    let generate_toc () =
        let wiki_pages =
            Directory.EnumerateFiles(Path.Combine(YAVSRG_PATH, "interlude", "docs", "wiki"))
            |> Seq.filter (fun f -> f.EndsWith ".md" && not (f.EndsWith "index.md"))
            |> Seq.map parse_wiki_file
            |> Array.ofSeq

        let wiki_toc = wiki_pages |> Array.groupBy (fun p -> p.Folder) |> Map.ofSeq

        JSON.ToFile (Path.Combine(YAVSRG_PATH, "interlude", "docs", "wiki", "index.json"), true) wiki_toc
