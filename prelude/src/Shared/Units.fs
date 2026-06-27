namespace Prelude

type [<Measure>] ms = Percyqaz.Common.ms
type [<Measure>] rate = Percyqaz.Common.rate
type [<Measure>] beat
type [<Measure>] minute
type [<Measure>] second

type GameplayTime = float32<ms / rate>
type Rate = float32<rate>
type Time = Percyqaz.Common.Time

module Time =
    let inline of_number (f: ^T) : Time = float32 f * 1.0f<ms>
    let infinity : Time = infinityf * 1.0f<ms>

[<AutoOpen>]
module UnitConstants =

    let [<Literal>] MS_PER_MINUTE = 60_000.0f<ms / minute>
    let [<Literal>] LOWEST_SUPPORTED_RATE = 0.5f<rate>
    let [<Literal>] HIGHEST_SUPPORTED_RATE = 3.0f<rate>

module Setting =
    open Percyqaz.Common.Setting
    let rate x = x |> bounded (LOWEST_SUPPORTED_RATE, HIGHEST_SUPPORTED_RATE) |> roundf_uom 2