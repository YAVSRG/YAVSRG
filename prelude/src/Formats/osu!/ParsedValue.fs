namespace Prelude.Formats.Osu

open System
open System.Globalization
open System.Runtime.CompilerServices

[<Struct>]
type ParsedValue<'T> =
    | Valid of 'T
    | Invalid

exception ParseException of args: obj[]

type ParsedValueExtensions =

    [<Extension>]
    static member inline ValueAt(split_values: string array, index: int) : ParsedValue<string> =
        if index < split_values.Length then Valid(split_values.[index]) else Invalid

    [<Extension>]
    static member inline DefaultValue<'T>(pvalue: ParsedValue<'T>, default_value: 'T) : 'T =
        match pvalue with
        | Valid v -> v
        | Invalid -> default_value

    [<Extension>]
    static member inline ReplaceInvalidWith<'T>(pvalue: ParsedValue<'T>, default_value: 'T) : ParsedValue<'T> =
        match pvalue with
        | Valid v -> Valid v
        | Invalid -> Valid default_value

    [<Extension>]
    static member inline ExpectValid(pvalue: ParsedValue<'T>, [<ParamArray>] args: obj[]) : 'T =
        match pvalue with
        | Valid v -> v
        | Invalid -> raise(ParseException(args))

    [<Extension>]
    static member inline ParseDouble(pvalue: ParsedValue<string>) : ParsedValue<float> =
        match pvalue with
        | Valid v ->
            match Double.TryParse(v.Trim(), CultureInfo.InvariantCulture) with
            | true, parsed -> Valid parsed
            | false, _ -> Invalid
        | Invalid -> Invalid

    [<Extension>]
    static member inline ParseFloat(pvalue: ParsedValue<string>) : ParsedValue<float32> =
        match pvalue with
        | Valid v ->
            match Single.TryParse(v.Trim(), CultureInfo.InvariantCulture) with
            | true, parsed -> Valid parsed
            | false, _ -> Invalid
        | Invalid -> Invalid

    [<Extension>]
    static member inline ParseInt(pvalue: ParsedValue<string>) : ParsedValue<int> =
        match pvalue with
        | Valid v ->
            match Int32.TryParse(v.Trim(), CultureInfo.InvariantCulture) with
            | true, parsed -> Valid parsed
            | false, _ -> Invalid
        | Invalid -> Invalid

    [<Extension>]
    static member RejectInfinity(pvalue: ParsedValue<float>) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if Double.IsInfinity(v) then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectInfinity(pvalue: ParsedValue<float32>) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if Single.IsInfinity(v) then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectNan(pvalue: ParsedValue<float>) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if Double.IsNaN(v) then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectNan(pvalue: ParsedValue<float32>) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if Single.IsNaN(v) then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceZeroWith(pvalue: ParsedValue<float>, replacement_value: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v = 0.0 then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceZeroWith(pvalue: ParsedValue<float32>, replacement_value: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v = 0.0f then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceZeroWith(pvalue: ParsedValue<int>, replacement_value: int) : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v = 0 then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceNanWith(pvalue: ParsedValue<float>, replacement_value: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if Double.IsNaN(v) then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceNanWith(pvalue: ParsedValue<float32>, replacement_value: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if Single.IsNaN(v) then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeInclusiveWith
        (pvalue: ParsedValue<float>, lo: float, hi: float, replacement_value: float)
        : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeInclusiveWith
        (pvalue: ParsedValue<float32>, lo: float32, hi: float32, replacement_value: float32)
        : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeInclusiveWith
        (pvalue: ParsedValue<int>, lo: int, hi: int, replacement_value: int)
        : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeWith
        (pvalue: ParsedValue<float>, lo: float, hi: float, replacement_value: float)
        : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeWith
        (pvalue: ParsedValue<float32>, lo: float32, hi: float32, replacement_value: float32)
        : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceOutOfRangeWith
        (pvalue: ParsedValue<int>, lo: int, hi: int, replacement_value: int)
        : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectOutOfRange(pvalue: ParsedValue<float>, lo: float, hi: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectOutOfRange(pvalue: ParsedValue<float32>, lo: float32, hi: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member RejectOutOfRange(pvalue: ParsedValue<int>, lo: int, hi: int) : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Invalid else Valid(v)
        | Invalid -> Invalid

    [<Extension>]
    static member ClampBetween(pvalue: ParsedValue<float>, lo: float, hi: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> Valid(v |> max lo |> min hi)
        | Invalid -> Invalid

    [<Extension>]
    static member ClampBetween(pvalue: ParsedValue<float32>, lo: float32, hi: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> Valid(v |> max lo |> min hi)
        | Invalid -> Invalid

    [<Extension>]
    static member ClampBetween(pvalue: ParsedValue<int>, lo: int, hi: int) : ParsedValue<int> =
        match pvalue with
        | Valid v -> Valid(v |> max lo |> min hi)
        | Invalid -> Invalid

    [<Extension>]
    static member TruncateToInt(pvalue: ParsedValue<float32>) : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v >= float32 Int32.MinValue && v <= float32 Int32.MaxValue then Valid(int v) else Invalid
        | Invalid -> Invalid

    [<Extension>]
    static member ReplaceWhitespaceWith(pvalue: ParsedValue<string>, replacement_value: string) : ParsedValue<string> =
        match pvalue with
        | Valid v -> if v.Trim() = "" then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
