namespace Prelude

open System

/// Pseudorandom number generator for use in modifiers for charts
/// Its purpose:
///   Be satisfactorily 'random' feeling for gameplay
///   Be so simple you could reimplement it in any language without needing to speak with me (if ever needed)
///   Be future proof, it will not change like Microsoft's System.Random could and so a score with Interlude's Random mod set in 2024 can be understood in 2034

type PseudoRandom =
    private {
        mutable State: uint64
    }

    member private this.Step() =
        this.State <-
            if this.State &&& 1uL <> 0uL then
                (this.State >>> 1) ^^^ 0x800000000000000DuL
            else
                this.State >>> 1

    /// Returns number in the range [0 .. max)
    member this.Next(max: int) : int =
        for i = 1 to 8 do
            this.Step()

        let as_int = int32 this.State
        let non_negative = if as_int = Int32.MinValue then Int32.MaxValue else abs as_int
        non_negative % max

    /// Returns number in the range [min .. max)
    member this.Next(min: int, max: int) = this.Next (max - min) + min

    member this.Shuffle<'T>(items: 'T array) : unit =
        for i = items.Length - 1 downto 1 do
            let j = this.Next(i)
            let swap = items.[i]
            items.[i] <- items.[j]
            items.[j] <- swap

    static member FromSeed (seed: int32) =
        let non_zero_64_bit_seed : uint64 =
            if seed = 0 then 31415926uL
            elif seed = Int32.MinValue then Int32.MaxValue |> uint64
            elif seed < 0 then abs seed |> uint64
            else seed |> uint64

        { State = non_zero_64_bit_seed }