namespace Prelude

open System.IO
open System.Linq
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

module Json =
    
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

    module JsonHelper = 
        let s = new JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore, DefaultValueHandling = DefaultValueHandling.IgnoreAndPopulate) |> addConverters |> JsonSerializer.Create

        let load<'T> string : 'T = 
            use jr = new JsonTextReader(new StringReader(string))
            s.Deserialize<'T> jr

        let save<'T> (obj : 'T) : string = 
            use sw = new StringWriter()
            use jw = new JsonTextWriter(sw)
            s.Serialize(jw, obj)
            sw.ToString()

        let loadFile<'T> (path : string) : 'T =
            use jr = new JsonTextReader(new StreamReader(path))
            s.Deserialize<'T> jr
    
        let saveFile<'T> (obj : 'T) (path : string) : unit = 
            use sw = new StreamWriter(path)
            use jw = new JsonTextWriter(sw)
            s.Serialize(jw, obj)