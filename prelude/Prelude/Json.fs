module Prelude.Json

open Newtonsoft.Json
open Microsoft.FSharp.Reflection
open System.Linq
open Newtonsoft.Json.Linq

type TupleConverter() =
    inherit JsonConverter()

    override this.CanConvert t = FSharpType.IsTuple t

    override this.WriteJson(writer, value, serializer) =
        serializer.Serialize(writer, FSharpValue.GetTupleFields value)

    override this.ReadJson(reader, t, existingValue, serializer) =
        let argTypes = FSharpType.GetTupleElements t
        let array = serializer.Deserialize<JArray>(reader)
        let items = array.Select(fun a i -> a.ToObject(argTypes.[i], serializer)).ToArray()
        FSharpValue.MakeTuple(items, t)

let addConverters (settings : JsonSerializerSettings) : JsonSerializerSettings = 
    settings.Converters.Add(TupleConverter())
    settings