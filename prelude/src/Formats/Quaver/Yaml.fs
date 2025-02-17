namespace Prelude.Formats.Quaver

open System

module Yaml =

    type ParsedYamlNestedObject = Map<string, string>
    type ParsedYamlValue = String of string | Array of ResizeArray<ParsedYamlNestedObject>
    type ParsedYamlObject = Map<string, ParsedYamlValue>

    let rec private parse_nested_list (lines: string list) : ResizeArray<ParsedYamlNestedObject> * string list =
        let mutable lines_remaining = lines

        let mutable object : ParsedYamlNestedObject = Map.empty
        let parsed_objects = ResizeArray<ParsedYamlNestedObject>()

        let mutable end_of_list = lines_remaining = []

        let indent = (List.head lines_remaining).IndexOf("-")

        while lines_remaining <> [] && not end_of_list do
            let line = List.head lines_remaining
            lines_remaining <- List.tail lines_remaining

            let is_indented = line.StartsWith(" ") || line.StartsWith ("-")
            let is_new_item = line.Trim().StartsWith("-")

            if not is_indented then
                end_of_list <- true
                lines_remaining <- line :: lines_remaining
            elif is_new_item && line.IndexOf("-") <> indent then
                end_of_list <- true
                lines_remaining <- line :: lines_remaining
            else
                if is_new_item && object <> Map.empty then
                    parsed_objects.Add object
                    object <- Map.empty

                let key_value = line.Trim().TrimStart('-').Trim().Split(":", 2)

                if key_value.[0] = "{}" then
                    ()
                elif key_value.Length > 1 && key_value.[1] <> "" then
                    object <- Map.add key_value.[0] (key_value.[1].Trim()) object
                else
                    let _, new_lines_remaining = parse_nested_list lines_remaining
                    lines_remaining <- new_lines_remaining

        parsed_objects.Add object
        parsed_objects, lines_remaining

    let from_lines (lines: string list) : ParsedYamlObject =
        let mutable object : ParsedYamlObject = Map.empty
        let mutable lines_remaining = lines
        while lines_remaining <> [] do
            let line = List.head lines_remaining
            lines_remaining <- List.tail lines_remaining

            let key_value = line.Trim().Split(':', 2)
            if key_value.Length > 1 && key_value.[1] <> "" then
                let stripped_value = key_value.[1].Trim().Trim('\'')
                let value = if stripped_value = "[]" then Array (ResizeArray<ParsedYamlNestedObject>()) else String stripped_value
                object <- Map.add key_value.[0] value object
            else
                let nested, ls = parse_nested_list lines_remaining
                object <- Map.add key_value.[0] (Array nested) object
                lines_remaining <- ls

        object

    let get_string (key: string) (parsed: ParsedYamlObject) : string =
        match parsed.TryFind key with
        | Some (String s) -> s
        | x -> failwithf "Expected string present for key '%s' but got %A" key x

    let get_string_or (key: string) (otherwise: string) (parsed: ParsedYamlObject) : string =
        match parsed.TryFind key with
        | Some (String s) -> s
        | x -> otherwise

    let get_int (key: string) (parsed: ParsedYamlObject) : int =
        match parsed.TryFind key with
        | Some (String s) ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | x -> failwithf "Expected (integer valued) string present for key '%s' but got %A" key x

    let get_int_or (key: string) (otherwise: int) (parsed: ParsedYamlObject) : int =
        match parsed.TryFind key with
        | Some (String s) ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | x -> otherwise

    let get_array<'T> (key: string) (parser: ParsedYamlNestedObject -> 'T) (parsed: ParsedYamlObject) : 'T list =
        match parsed.TryFind key with
        | Some (Array xs) -> Seq.map parser xs |> List.ofSeq
        | x -> failwithf "Expected array present for key '%s' but got %A" key x

    let get_nested_float (key: string) (parsed: ParsedYamlNestedObject) : float32 =
        match parsed.TryFind key with
        | Some s ->
            match Single.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected float '%s' for key '%s'" s key
        | None -> failwithf "Expected string present for key '%s'" key

    let get_nested_int (key: string) (parsed: ParsedYamlNestedObject) : int =
        match parsed.TryFind key with
        | Some s ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | None -> failwithf "Expected string present for key '%s'" key

    let get_nested_float_or (key: string) (otherwise: float32) (parsed: ParsedYamlNestedObject) : float32 =
        match parsed.TryFind key with
        | Some s ->
            match Single.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected float '%s' for key '%s'" s key
        | None -> otherwise

    let get_nested_int_or (key: string) (otherwise: int) (parsed: ParsedYamlNestedObject) : int =
        match parsed.TryFind key with
        | Some s ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | None -> otherwise