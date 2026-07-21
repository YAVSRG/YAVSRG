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
    static member ReplaceOutOfRangeInclusiveWith(pvalue: ParsedValue<float>, lo: float, hi: float, replacement_value: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
        
    [<Extension>]
    static member ReplaceOutOfRangeInclusiveWith(pvalue: ParsedValue<float32>, lo: float32, hi: float32, replacement_value: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
        
    [<Extension>]
    static member ReplaceOutOfRangeInclusiveWith(pvalue: ParsedValue<int>, lo: int, hi: int, replacement_value: int) : ParsedValue<int> =
        match pvalue with
        | Valid v -> if v <= lo || v >= hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
        
    [<Extension>]
    static member ReplaceOutOfRangeWith(pvalue: ParsedValue<float>, lo: float, hi: float, replacement_value: float) : ParsedValue<float> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
        
    [<Extension>]
    static member ReplaceOutOfRangeWith(pvalue: ParsedValue<float32>, lo: float32, hi: float32, replacement_value: float32) : ParsedValue<float32> =
        match pvalue with
        | Valid v -> if v < lo || v > hi then Valid(replacement_value) else Valid(v)
        | Invalid -> Invalid
        
    [<Extension>]
    static member ReplaceOutOfRangeWith(pvalue: ParsedValue<int>, lo: int, hi: int, replacement_value: int) : ParsedValue<int> =
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
        | Valid v ->
            if v >= float32 Int32.MinValue && v <= float32 Int32.MaxValue then
                Valid(int v)
            else
                Invalid
        | Invalid -> Invalid
    
[<Struct>]
type SplitValues =
    { Original: string; Values: string array }
    
    static member Parse(text: string, separator: char) : SplitValues =
        { Original = text; Values = text.Split(separator) }
        
    member inline this.Length = this.Values.Length
    member inline this.ToArray() : string array = this.Values
    member this.Item with get(index: int) = this.Values.[index]
    
    member this.String(index: int) : string =
        if index >= this.Length then
            failwithf "no string value at position %i: %s" index this.Original
        else
            this.[index].Trim().Trim('"')
        
    member this.StringOrDefault(index: int, default_value: string) : string =
        if index >= this.Length then
            default_value
        else
            this.[index].Trim().Trim('"')
            
    member this.StringOrDefault(index: int) : string = this.StringOrDefault(index, "")
            
    member this.UntrimmedStringOrDefault(index: int, default_value: string) : string =
        if index >= this.Length then default_value else this.[index]

    member this.Int(index: int) : int =
        if index >= this.Length then
            failwithf "no int value at position %i: %s" index this.Original
        else
            match Double.TryParse(this.[index].Trim(), CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= Int32.MinValue -> int v
            | _ -> failwithf "invalid int value at position %i: %s" index this.Original

    member this.IntOrDefault(index: int, default_value: int) : int =
        if index >= this.Length then
            default_value
        else
            match Double.TryParse(this.[index].Trim(), CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= Int32.MinValue -> int v
            | _ -> default_value
            
    member this.IntOrDefault(index: int) : int = this.IntOrDefault(index, 0)

    member this.Float(index: int) : float =
        if index >= this.Length then
            failwithf "no float value at position %i: %s" index this.Original
        else
            match Double.TryParse(this.[index].Trim(), CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= Int32.MinValue -> v
            | _ -> failwithf "invalid int value at position %i: %s" index this.Original

    member this.FloatOrDefault(index: int, default_value: float, allow_infinity: bool) : float =
        if index >= this.Length then
            default_value
        else
            match Double.TryParse(this.[index].Trim(), CultureInfo.InvariantCulture) with
            | true, v when allow_infinity || (v <= Int32.MaxValue && v >= Int32.MinValue) -> v
            | _ -> default_value
            
    member inline this.FloatOrDefault(index: int, default_value: float) : float =
        this.FloatOrDefault(index, default_value, false)
    
    member inline this.FloatOrDefault(index: int) : float =
        this.FloatOrDefault(index, 0.0, false)

    member this.Enum<'T
        when 'T :> Enum
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType>(index: int) : 'T =
        if index >= this.Length then
            failwithf "no enum value at position %i: %s" index this.Original
        else
            match Enum.TryParse(this.[index].Trim(), true) with
            | true, v -> v
            | false, _ -> failwithf "invalid enum value at position %i: %s" index this.Original

    member this.EnumOrDefault<'T
        when 'T :> Enum
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType>(index: int, default_value: 'T, allow_undefined: bool) : 'T =
        if index >= this.Length then
            default_value
        else
            match Enum.TryParse(this.[index].Trim(), true) with
            | true, v when allow_undefined || Enum.IsDefined(v) -> v
            | _ -> default_value