namespace YAVSRG.CLI

open System
open System.Drawing
open System.Text.RegularExpressions
open System.IO

module Formatting =

    let run_fantomas () : unit =
        Shell.Exec("dotnet", "fantomas .") |> ignore

    let simple_view (file_contents: string) : unit =
        let regex =
            Regex("[\t ]*(module .+? =|type .+? =|let .+? =|member .+? =|override .+? =)")

        let span = file_contents.AsSpan()

        for m in regex.EnumerateMatches(span) do
            let match_span = span.Slice(m.Index, m.Length)
            printfn "%s" (String match_span)

    let simple_view_all () : unit =
        for filename, file_contents in SourceFiles.walk_fs_file_contents YAVSRG_PATH do
            printfn "%s\n====\n" filename
            simple_view(file_contents)

    let check_linecounts () : unit =
        let mutable loc = 0

        for filename, file_contents in SourceFiles.walk_fs_file_contents YAVSRG_PATH do
            let lines = file_contents.Split('\n').Length

            if lines > 300 then
                printfn "%s has %i lines" filename lines

            loc <- loc + lines

        printfn "total %i lines of f#" loc

    let check_filenames () =
        SourceFiles.walk_fs_files YAVSRG_PATH
        |> Seq.map Path.GetFileName
        |> Seq.countBy id
        |> Seq.sortByDescending snd
        |> Seq.truncate 10
        |> Seq.iter(fun (file_name, count) -> printfn "%s: %i uses" file_name count)
