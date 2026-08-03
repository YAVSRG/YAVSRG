namespace Prelude.Formats.Osu

open System
open System.Runtime.CompilerServices

type MapExtensions =

    [<Extension>]
    static member Value(map: Map<string, string>, key: string) : ParsedValue<string> =
        match map.TryFind(key) with
        | Some value -> Valid(value)
        | None -> Invalid

    [<Extension>]
    static member StringOrDefault(map: Map<string, string>, key: string, default_value: string) : string =
        map.Value(key).DefaultValue(default_value)

    [<Extension>]
    static member IntOrDefault(map: Map<string, string>, key: string, default_value: int) : int =
        map.Value(key).ParseInt().DefaultValue(default_value)

    [<Extension>]
    static member TryInt(map: Map<string, string>, key: string) : int option = map.Value(key).ParseInt().ToOption()

    [<Extension>]
    static member DoubleOrDefault(map: Map<string, string>, key: string, default_value: float) : float =
        map.Value(key).ParseDouble().DefaultValue(default_value)

    [<Extension>]
    static member TryDouble(map: Map<string, string>, key: string) : float option =
        map.Value(key).ParseDouble().ToOption()

    [<Extension>]
    static member inline EnumOrDefault(map: Map<string, string>, key: string, default_value: 'T) : 'T =
        match map.TryFind(key) with
        | Some s ->
            match Enum.TryParse(s, true) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value
