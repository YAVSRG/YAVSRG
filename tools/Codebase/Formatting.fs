namespace YAVSRG.CLI

open System
open System.Drawing
open System.Text.RegularExpressions
open System.IO

type FormattingMessageId =
    | Q0001
    | Q0002
    override this.ToString() : string =
        sprintf "%A: %s" this this.Message
    member this.Message : string =
        match this with
        | Q0001 -> "function arguments missing type annotation"
        | Q0002 -> "camel case used for identifier"

module Formatting =
    
    type Message =
        { FilePath: string; Location: (int * int) option; Id: FormattingMessageId }
        override this.ToString() : string =
            match this.Location with
            | Some (line, column) -> sprintf "%s(%i,%i): %O" this.FilePath line column this.Id
            | None -> sprintf "%s: %A" this.FilePath this.Id
            
        member this.Open() : unit =
            let launch_rider_with_args(args: string) =
                Shell.Exec(@"C:\Program Files\JetBrains\JetBrains Rider 2026.1\bin\rider64.exe", args)
                
            match this.Location with
            | Some (line, column) -> sprintf "--line %i --column %i %s" line column this.FilePath
            | None -> this.FilePath
            |> launch_rider_with_args

    let run_fantomas () = Shell.Exec("fantomas", ".")
        
    let inline color_text(fg: Color, bg: Color, text: string) : string =
        sprintf "\u001b[38;2;%d;%d;%d;48;2;%d;%d;%dm%s\u001b[0m" fg.R fg.G fg.B bg.R bg.G bg.B text
    
    let inline create_regex (regex_string: string) : Regex =
        Regex(regex_string.Replace("$IDENT", "[A-Za-z0-9_]+"))
    
    let check_file (file_path: string) : Message seq =
        let file_text = File.ReadAllText(file_path)
        
        let index_to_line_number(index: int) : int * int =
            let rec loop (current_line: int) (current_index: int) =
                let next_index = file_text.IndexOf('\n', current_index)
                if next_index > index || next_index < 0 then
                    current_line, (index - current_index) + 1
                else loop (current_line + 1) (next_index + 1)
            loop 1 0
        
        let regex_warnings(regex: Regex, warning: FormattingMessageId) =
            seq {
                for regex_match in regex.Matches(file_text) do
                    let line_n, line_pos = index_to_line_number(regex_match.Index + 1)
                    yield { FilePath = file_path; Location = Some(line_n, line_pos); Id = warning }
            }
        
        seq {
            let missing_arg_type_annotations = create_regex("\slet(?>( rec| mutable)?)(?>( internal| private)?)(?>( inline)?) $IDENT $IDENT")
            yield! regex_warnings(missing_arg_type_annotations, Q0001)
                
            //let camel_case_symbol = create_regex("[\(\s][a-z]+[A-Z]$IDENT")
            //yield! regex_warnings(camel_case_symbol, Q0002)
        }
        
    let check_files () : Message array =
        SourceFiles.walk_fs_files(YAVSRG_PATH)
        |> Seq.collect check_file
        |> Array.ofSeq
            
    let browse_formatting_messages(messages: Message array) : unit =
        let HEIGHT = Console.WindowHeight
        let SCROLLOFF = HEIGHT / 2
        let mutable position = 0
        let mutable selection = 0
        let mutable loop = messages.Length > 0
        while loop do
            Console.Clear()
            
            position <- max 0 (selection - SCROLLOFF) |> min (messages.Length - HEIGHT)
            for i = position to (position + HEIGHT) - 1 do
                if i = selection then color_text(Color.LightGreen, Color.FromArgb(0x333333), messages.[i].ToString())
                else messages.[i].ToString()
                |> Console.WriteLine
            
            match Console.ReadKey(true).Key with
            | ConsoleKey.Escape -> loop <- false
            | ConsoleKey.DownArrow
            | ConsoleKey.J -> selection <- (selection + 1) % messages.Length
            | ConsoleKey.UpArrow
            | ConsoleKey.K -> selection <- (selection + messages.Length - 1) % messages.Length
            | ConsoleKey.Enter -> messages.[selection].Open()
            | _ -> ()

    let simple_view (file_contents: string) =
        let regex = Regex("[\t ]*(module .+? =|type .+? =|let .+? =|member .+? =|override .+? =)")
        let span = file_contents.AsSpan()
        for m in regex.EnumerateMatches span do
            let match_span = span.Slice(m.Index, m.Length)
            printfn "%s" (String match_span)

    let simple_view_all () =
        for filename, file_contents in SourceFiles.walk_fs_file_contents YAVSRG_PATH do
            printfn "%s\n====\n" filename
            simple_view file_contents

    let check_linecounts () =
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
        |> Seq.iter (fun (file_name, count) -> printfn "%s: %i uses" file_name count)