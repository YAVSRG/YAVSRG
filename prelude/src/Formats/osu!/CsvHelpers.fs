namespace Prelude.Formats.Osu

open System
open System.Globalization

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