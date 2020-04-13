namespace Prelude

open System
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

    type UnionConverter() =
        inherit JsonConverter()

        override this.CanConvert t = FSharpType.IsUnion t

        override this.WriteJson(writer, value, serializer) =
            let t = value.GetType()
            let caseInfo, fieldValues = FSharpValue.GetUnionFields(value, t)
            writer.WriteStartObject()
            writer.WritePropertyName(caseInfo.Name)
            let value =
                match fieldValues.Length with
                | 0 -> null
                | 1 -> fieldValues.[0]
                | _ -> fieldValues :> obj
            serializer.Serialize(writer, value)
            writer.WriteEndObject()

        override this.ReadJson(reader, t, existingValue, serializer) =
            reader.Read() |> ignore
            if reader.TokenType <> JsonToken.PropertyName then failwith "no property name found for union"
            let name = reader.Value :?> string
            let case, fields =
                match FSharpType.GetUnionCases(t) |> Array.filter (fun case -> case.Name = name) with
                | [|case|] -> case, case.GetFields()
                | _ -> failwith "could not deserialise unknown union case"
            printfn "matched case: %s with %i fields" case.Name fields.Length
            reader.Read() |> ignore
            let result =
                match fields.Length with
                | 0 ->
                    //there should be a null token under the cursor
                    FSharpValue.MakeUnion(case, [||])
                | 1 ->
                    printfn "reading type: %A" fields.[0].PropertyType
                    let res = [| serializer.Deserialize(reader, fields.[0].PropertyType) |]
                    printfn "read field: %A" res
                    FSharpValue.MakeUnion(case, res)
                | _ ->
                    let tupleType =
                        fields
                        |> Seq.map (fun f -> f.PropertyType)
                        |> Seq.toArray
                        |> FSharpType.MakeTupleType
                    let tuple = serializer.Deserialize(reader, tupleType)
                    FSharpValue.MakeUnion(case, FSharpValue.GetTupleFields(tuple))
            reader.Read() |> ignore
            if reader.TokenType <> JsonToken.EndObject then failwith "end of object"
            result

    type OptionConverter() =
        inherit JsonConverter()

        override this.CanConvert t = t.IsGenericType && typedefof<option<_>>.Equals(t.GetGenericTypeDefinition())

        override this.WriteJson(writer, value, serializer) =
            let value =
                match value = null with
                | true -> null
                | false ->
                    let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
            serializer.Serialize(writer, value)

        override this.ReadJson(reader, t, existingValue, serializer) =
            let innerType = t.GetGenericArguments().[0]

            let innerType = 
              if innerType.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType([| innerType |])
              else
                innerType

            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases t

            if value = null then
              FSharpValue.MakeUnion(cases.[0], [||])
            else
              FSharpValue.MakeUnion(cases.[1], [|value|])

    let addConverters (settings : JsonSerializerSettings) : JsonSerializerSettings = 
        settings.Converters.Add(TupleConverter())
        settings.Converters.Add(OptionConverter())
        settings.Converters.Add(UnionConverter())
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