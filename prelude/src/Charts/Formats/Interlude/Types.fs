namespace Prelude.Charts

open System
open System.Linq
open System.IO
open System.Collections.Generic
open System.Security.Cryptography
open Percyqaz.Data
open Percyqaz.Common
open Prelude

type NoteType =
    | NOTHING = 0uy
    | NORMAL = 1uy
    | HOLDHEAD = 2uy
    | HOLDBODY = 3uy
    | HOLDTAIL = 4uy

type NoteRow = NoteType array

module NoteRow =

    let clone = Array.copy

    let create_empty keycount : NoteRow = Array.create keycount NoteType.NOTHING

    let create_notes keycount (notes: Bitmask) =
        let nr = create_empty keycount

        for k in Bitmask.toSeq notes do
            nr.[k] <- NoteType.NORMAL

        nr

    let create_ln_bodies keycount (notes: Bitmask) =
        let nr = create_empty keycount

        for k in Bitmask.toSeq notes do
            nr.[k] <- NoteType.HOLDBODY

        nr

    let is_empty: NoteRow -> bool =
        Array.forall (
            function
            | NoteType.NOTHING
            | NoteType.HOLDBODY -> true
            | _ -> false
        )

    let read (keycount: int) (br: BinaryReader) : NoteRow =
        let row = create_empty keycount
        let columns = br.ReadUInt16()

        for k in Bitmask.toSeq columns do
            row.[k] <-
                match br.ReadByte() with
                | 1uy -> NoteType.NORMAL
                | 2uy -> NoteType.HOLDHEAD
                | 3uy -> NoteType.HOLDBODY
                | 4uy -> NoteType.HOLDTAIL
                | b -> failwithf "unexpected note type in chart data: %i" b

        row

    let write (bw: BinaryWriter) (row: NoteRow) =
        let columns =
            seq {
                for i in 0 .. row.Length - 1 do
                    if row.[i] <> NoteType.NOTHING then
                        yield i
            }

        bw.Write(Bitmask.ofSeq columns)

        for k in columns do
            bw.Write(byte row.[k])

    let pretty_print (row: NoteRow) =
        let p =
            function
            | NoteType.NORMAL -> '#'
            | NoteType.HOLDHEAD -> '^'
            | NoteType.HOLDBODY -> '|'
            | NoteType.HOLDTAIL -> 'v'
            | NoteType.NOTHING
            | _ -> ' '

        new string (row |> Array.map p)

type BPM =
    {
        Meter: int<beat>
        MsPerBeat: float32<ms / beat>
    }

type SV = float32