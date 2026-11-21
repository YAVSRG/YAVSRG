namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open Percyqaz.Common

[<AutoOpen>]
module internal OsuDbHelpers =

    let read_byte (br: BinaryReader) : byte = br.ReadByte()
    let read_short (br: BinaryReader) : int16 = br.ReadInt16()
    let read_int (br: BinaryReader) : int = br.ReadInt32()
    let read_long (br: BinaryReader) : int64 = br.ReadInt64()
    let read_single (br: BinaryReader) : float32 = br.ReadSingle()
    let read_double (br: BinaryReader) : float = br.ReadDouble()
    let read_bool (br: BinaryReader) : bool = br.ReadByte() <> 0x00uy

    let read_string (br: BinaryReader) : string =
        let b = br.ReadByte()

        if b = 0x00uy then ""
        elif b = 0x0buy then br.ReadString()
        else failwith "Unknown byte while reading string"

    let read_int_double_pair (br: BinaryReader) : int * float =
        if br.ReadByte() <> 0x08uy then
            failwith "Got unexpected byte at start of int-double pair"

        let int = read_int br

        let double =
            match br.ReadByte() with
            | 0x0Duy -> read_double br
            | 0x0Cuy -> read_single br |> float
            | other -> failwithf "Got unexpected byte %X for float part of int-double pair" other

        int, double

    let read_star_ratings (br: BinaryReader) : (int * float) array =
        let count = read_int br

        if count < 0 then
            failwith "Something has gone wrong reading star ratings (meaning misaligned bytes earlier on)"

        Array.init count (fun _ -> read_int_double_pair br)

    let write_string (bw: BinaryWriter) (s: string) =
        if s = "" then
            bw.Write(0x00uy)
        else
            bw.Write(0x0buy)
            bw.Write(s)

    let read_timing_point (br: BinaryReader) = br.ReadBytes(17) |> ignore

    let timestamp_0001_to_1601 (l: int64) = DateTime(l).ToFileTimeUtc()
