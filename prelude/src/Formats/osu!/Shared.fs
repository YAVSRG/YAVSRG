namespace Prelude.Formats.Osu

open System
open System.Globalization

module MapHelpers =

    let string_or (key: string) (default_value: string) (map: Map<string, string>) : string =
        map.TryFind key |> Option.defaultValue default_value

    let int_or (key: string) (default_value: int) (map: Map<string, string>) : int =
        match map.TryFind key with
        | Some s ->
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> (int v)
            | false, _ -> default_value
        | None -> default_value

    let int_opt (key: string) (map: Map<string, string>) : int option =
        match map.TryFind key with
        | Some s ->
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some (int v)
            | false, _ -> None
        | None -> None

    let float_or (key: string) (default_value: float) (map: Map<string, string>) : float =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

    let float_opt (key: string) (map: Map<string, string>) : float option =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None
        | None -> None

    let enum_or<'T
        when 'T : enum<int>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> (key: string) (default_value: 'T) (map: Map<string, string>) : 'T =
        match map.TryFind key with
        | Some s ->
            match Enum.TryParse(s, true) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

module CsvHelpers =

    let string_or (index: int) (default_value: string) (csv: string array) : string =
        if index >= csv.Length then
            default_value
        else
            csv.[index]

    let int_or (index: int) (default_value: int) (csv: string array) : int =
        if index >= csv.Length then
            default_value
        else
            match Single.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> int v
            | false, _ -> default_value

    let float_or (index: int) (default_value: float) (csv: string array) : float =
        if index >= csv.Length then
            default_value
        else
            match Double.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value

    let enum_or<'T
        when 'T : enum<int>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> (index: int) (default_value: 'T) (csv: string array) : 'T =
        if index >= csv.Length then
            default_value
        else
            match Enum.TryParse(csv.[index], true) with
            | true, v -> v
            | false, _ -> default_value