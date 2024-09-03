﻿namespace Prelude.Charts.Formats.osu

open System
open System.Globalization

module MapHelpers =

    let string_or (key: string) (default_value: string) (map: Map<string, string>) =
        map.TryFind key |> Option.defaultValue default_value

    let int_or (key: string) (default_value: int) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

    let int_opt (key: string) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None
        | None -> None

    let float_or (key: string) (default_value: float) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

    let float_opt (key: string) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None
        | None -> None

    let enum_or<'T when 'T : enum<int> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType> (key: string) (default_value: 'T) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Enum.TryParse(s, true) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

module CsvHelpers =
    
    let string_or (index: int) (default_value: string) (csv: string array) =
        if index >= csv.Length then 
            default_value 
        else
            csv.[index]

    let int_or (index: int) (default_value: int) (csv: string array) =
        if index >= csv.Length then 
            default_value
        else
            match Int32.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value

    let float_or (index: int) (default_value: float) (csv: string array) =
        if index >= csv.Length then 
            default_value
        else
            match Double.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value

    let enum_or<'T when 'T : enum<int> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType> (index: int) (default_value: 'T) (csv: string array) =
        if index >= csv.Length then 
            default_value 
        else
            match Enum.TryParse(csv.[index], true) with
            | true, v -> v
            | false, _ -> default_value