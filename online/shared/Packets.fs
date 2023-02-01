namespace Interlude.Web.Shared

open System
open System.IO

[<AutoOpen>]
module Packets =

    let PROTOCOL_VERSION = 0uy

    [<RequireQualifiedAccess>]
    type Upstream =
        | DISCONNECT
        | VERSION of byte
        | LOGIN of username: string
        | CHAT of message: string

        static member Read(kind: byte, data: byte array) : Upstream =
            use ms = new MemoryStream(data)
            use br = new BinaryReader(ms)
            let packet = 
                match kind with
                | 0x00uy -> DISCONNECT
                | 0x01uy -> VERSION (br.ReadByte())
                | 0x02uy -> LOGIN (br.ReadString())
                | 0x03uy -> CHAT (br.ReadString())
                | _ -> failwithf "Unknown packet type: %i" kind
            if ms.Position <> ms.Length then failwithf "Expected end-of-packet but there are %i extra bytes" (ms.Length - ms.Position)
            packet

        member this.Write() : byte * byte array =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            let kind = 
                match this with
                | DISCONNECT -> 0x00uy
                | VERSION v -> bw.Write v; 0x01uy
                | LOGIN name -> bw.Write name; 0x02uy
                | CHAT msg -> bw.Write msg; 0x03uy
            kind, ms.ToArray()
            
    [<RequireQualifiedAccess>]
    type Downstream =
        | DISCONNECT of reason: string
        | CHAT of sender: string * message: string
        | LOGIN_SUCCESS of username: string

        static member Read(kind: byte, data: byte array) : Downstream =
            use ms = new MemoryStream(data)
            use br = new BinaryReader(ms)
            let packet = 
                match kind with
                | 0x00uy -> DISCONNECT (br.ReadString())
                | 0x01uy -> CHAT (br.ReadString(), br.ReadString())
                | 0x02uy -> LOGIN_SUCCESS (br.ReadString())
                | _ -> failwithf "Unknown packet type: %i" kind
            if ms.Position <> ms.Length then failwithf "Expected end-of-packet but there are %i extra bytes" (ms.Length - ms.Position)
            packet

        member this.Write() : byte * byte array =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            let kind = 
                match this with
                | DISCONNECT reason -> bw.Write reason; 0x00uy
                | CHAT (sender, msg) -> bw.Write sender; bw.Write msg; 0x01uy
                | LOGIN_SUCCESS name -> bw.Write name; 0x02uy
            kind, ms.ToArray()