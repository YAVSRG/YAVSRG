namespace Prelude.Calculator.Patterns

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Calculator.Patterns

[<AutoOpen>]
module CorePatternExtensions =

    type CorePattern with
        member this.Write (bw: BinaryWriter) =
            match this with
            | Jacks -> bw.Write 0uy
            | Chordstream -> bw.Write 1uy
            | Stream -> bw.Write 2uy
        static member Read (br: BinaryReader) =
            match br.ReadByte() with
            | 0uy -> Jacks
            | 1uy -> Chordstream
            | 2uy -> Stream
            | unexpected -> failwithf "Unexpected byte '%x'" unexpected

// todo: wonder about making LN sections separate to the main pattern
// 7k files like to have a big LN spam at the end that should register separately
type LibraryPatternCluster =
    {
        Pattern: CorePattern
        BPM: int<beat / minute / rate>
        Mixed: bool
        Amount: Time
        Difficulty100: float32
        Difficulty150: float32

        LN10: float32
        LN25: float32
        LN50: float32
        LN75: float32
        LN90: float32

        Variety10: float32
        Variety25: float32
        Variety50: float32
        Variety75: float32
        Variety90: float32

        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density
    }

    member this.Write (bw: BinaryWriter) =
        this.Pattern.Write bw
        bw.Write (int this.BPM)
        bw.Write this.Mixed
        bw.Write (float32 this.Amount)
        bw.Write this.Difficulty100
        bw.Write this.Difficulty150

        bw.Write this.LN10
        bw.Write this.LN25
        bw.Write this.LN50
        bw.Write this.LN75
        bw.Write this.LN90

        bw.Write this.Variety10
        bw.Write this.Variety25
        bw.Write this.Variety50
        bw.Write this.Variety75
        bw.Write this.Variety90

        bw.Write (float32 this.Density10)
        bw.Write (float32 this.Density25)
        bw.Write (float32 this.Density50)
        bw.Write (float32 this.Density75)
        bw.Write (float32 this.Density90)

/// Precalculated and stored for every chart
/// Enough data to find similarities for endless mode, for groupings, etc
/// Data is general enough to estimate similarities and difficulties on rates
type LibraryPatternInfo =
    {
        Difficulty100: float32
        Difficulty150: float32
        SVAmount: Time
        Duration: Time

        MainPatterns: LibraryPatternCluster array
        HoldNotePercent: float32
        Purity: float32
        Simplicity: float32

        // have to stay for now for combined ratings - prepare to be destroyed!
        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density
    }

    member this.Write (bw: BinaryWriter) : unit =
        bw.Write this.Difficulty100
        bw.Write this.Difficulty150
        bw.Write (float32 this.SVAmount)
        bw.Write (float32 this.Duration)

        bw.Write this.MainPatterns.Length
        for p in this.MainPatterns do
            p.Write bw

        bw.Write (float32 this.HoldNotePercent)
        bw.Write this.Purity
        bw.Write this.Simplicity

        bw.Write (float32 this.Density10)
        bw.Write (float32 this.Density25)
        bw.Write (float32 this.Density50)
        bw.Write (float32 this.Density75)
        bw.Write (float32 this.Density90)