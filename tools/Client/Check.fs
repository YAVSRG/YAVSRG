namespace YAVSRG.CLI

open System
open System.Text.RegularExpressions
open YAVSRG.CLI.Utils

module Check =

    let simple_view (file_contents: string) =
        let regex = Regex("[\t ]*(module .+? =|type .+? =|let .+? =|member .+? =|override .+? =)")
        let span = file_contents.AsSpan()
        for m in regex.EnumerateMatches span do
            let match_span = span.Slice(m.Index, m.Length)
            printfn "%s" (String match_span)

    let simple_view_all () =
        for filename, file_contents in walk_fs_files YAVSRG_PATH do
            printfn "%s\n====\n" filename
            simple_view file_contents

    let check_linecounts () =
        let mutable loc = 0
        for filename, file_contents in walk_fs_files YAVSRG_PATH do
            let lines = file_contents.Split('\n').Length

            if lines > 300 then
                printfn "%s has %i lines" filename lines
            loc <- loc + lines
        printfn "total %i lines of f#" loc

    let format_all_code () = exec "fantomas" "."