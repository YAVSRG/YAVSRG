namespace Prelude

type [<Struct>] Bitmask =
    { Value: uint16 }

    static member Empty = { Value = 0us }

    member inline this.IsEmpty = this.Value = 0us

    member inline this.Contains(key: int) = (1us <<< key) &&& this.Value > 0us
    member inline this.Add(key: int) = { Value = (1us <<< key) ||| this.Value }
    member inline this.Remove(key: int) = { Value = ~~~(1us <<< key) &&& this.Value }
    member inline this.Toggle(key: int) = { Value = (1us <<< key) ^^^ this.Value }

    member inline this.Subtract(bitmask: Bitmask) = { Value = this.Value &&& ~~~bitmask.Value }

    member inline this.Iter([<InlineIfLambda>] action: int -> unit) =
        let mutable bits = this.Value
        let mutable i = 0
        while bits <> 0us do
            if bits &&& 1us <> 0us then
                action(i)
            i <- i + 1
            bits <- bits >>> 1

    member inline this.Count =
        let mutable count = 0
        this.Iter(fun _ -> count <- count + 1)
        count

    member inline this.ToSeq() =
        let mutable bits = this.Value
        let mutable i = 0
        seq {
            while bits <> 0us do
                if bits &&& 1us <> 0us then
                    yield i
                i <- i + 1
                bits <- bits >>> 1
        }

    static member inline FromSeq(keys: int seq) =
        Seq.fold (fun (bitmask: Bitmask) (key: int) -> bitmask.Add key) Bitmask.Empty keys

    member inline this.ToInt16() : uint16 =
        this.Value

    static member FromInt16(value: uint16) =
        { Value = value }