namespace Prelude

open System.IO
open System.Collections.Generic
open FSharp.Json

module Json =
    
    type DictionaryTransform<'T>() =
        interface ITypeTransform with
            member x.targetType() = typeof<Map<string, 'T>>
            member x.toTargetType value = value :?> Dictionary<string, 'T> |> Seq.map (|KeyValue|) |> Map.ofSeq :> obj
            member x.fromTargetType value = value :?> Map<string, 'T> |> Dictionary :> obj

    type ListTransform<'T>() =
        interface ITypeTransform with
            member x.targetType() = typeof<'T list>
            member x.toTargetType value = value :?> List<'T> |> List.ofSeq :> obj
            member x.fromTargetType value = value :?> 'T list |> ResizeArray :> obj

    module JsonHelper = 

        let load string : 'T =
            string
            |> Json.deserialize
    
        let save (obj : 'T) : string =
            obj
            |> Json.serializeU
    
        let loadFile (path : string) : 'T =
            use sr = new StreamReader(path)
            load (sr.ReadToEnd())
            
        let saveFile (obj : 'T) (path : string) : unit = 
            use sw = new StreamWriter(path)
            sw.Write(save obj)