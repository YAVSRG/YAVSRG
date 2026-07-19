namespace Prelude.Formats.Osu

open System
open System.Globalization

[<Struct>]
type SplitValues =
    { Original: string; Values: string array }
    
    static member Parse(text: string, separator: char) : SplitValues =
        { Original = text; Values = text.Split(separator, StringSplitOptions.TrimEntries) }
        
    member inline this.Length = this.Values.Length
    
    member inline this.ToArray() : string array = this.Values
        
    member this.Item with get(index: int) = this.Values.[index]
    
    member this.String(index: int) : string =
        if index >= this.Values.Length then
            failwithf "no string value at position %i: %s" index this.Original
        else
            this.Values.[index].Trim('"')
        
    member this.StringOrDefault(index: int, default_value: string) : string =
        if index >= this.Values.Length then
            default_value
        else
            this.Values.[index].Trim('"')
            
    member this.StringOrDefault(index: int) : string = this.StringOrDefault(index, "")

    member this.Int(index: int) : int =
        if index >= this.Values.Length then
            failwithf "no int value at position %i: %s" index this.Original
        else
            match Double.TryParse(this.Values.[index], CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= -Int32.MaxValue -> int v
            | _ -> failwithf "invalid int value at position %i: %s" index this.Original

    member this.IntOrDefault(index: int, default_value: int) : int =
        if index >= this.Values.Length then
            default_value
        else
            match Double.TryParse(this.Values.[index], CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= -Int32.MaxValue -> int v
            | _ -> default_value
            
    member this.IntOrDefault(index: int) : int = this.IntOrDefault(index, 0)

    member this.Float(index: int) : float =
        if index >= this.Values.Length then
            failwithf "no float value at position %i: %s" index this.Original
        else
            match Double.TryParse(this.Values.[index], CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= -Int32.MaxValue -> v
            | _ -> failwithf "invalid int value at position %i: %s" index this.Original

    member this.FloatOrDefault(index: int, default_value: float) : float =
        if index >= this.Values.Length then
            default_value
        else
            match Double.TryParse(this.Values.[index], CultureInfo.InvariantCulture) with
            | true, v when v <= Int32.MaxValue && v >= -Int32.MaxValue -> v
            | _ -> default_value
            
    member this.FloatOrDefault(index: int) : float = this.FloatOrDefault(index, 0.0)

    member this.Enum<'T
        when 'T : enum<int>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType>(index: int) : 'T =
        if index >= this.Values.Length then
            failwithf "no enum value at position %i: %s" index this.Original
        else
            match Enum.TryParse(this.Values.[index], true) with
            | true, v -> v
            | false, _ -> failwithf "invalid enum value at position %i: %s" index this.Original

    member this.EnumOrDefault<'T
        when 'T : enum<int>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType>(index: int, default_value: 'T) : 'T =
        if index >= this.Values.Length then
            default_value
        else
            match Enum.TryParse(this.Values.[index], true) with
            | true, v -> v
            | false, _ -> default_value