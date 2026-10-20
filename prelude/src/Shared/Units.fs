namespace Prelude

[<Measure>]
type ms = Percyqaz.Common.ms

[<Measure>]
type rate = Percyqaz.Common.rate

[<Measure>]
type beat

[<Measure>]
type minute

[<Measure>]
type second

type GameplayTime = float32<ms / rate>
type Rate = float32<rate>
type Time = Percyqaz.Common.Time

module Time =
    let inline of_number (f: ^T) : Time = float32 f * 1.0f<ms>
    let infinity: Time = infinityf * 1.0f<ms>

[<AutoOpen>]
module UnitConstants =

    [<Literal>]
    let MS_PER_MINUTE = 60_000.0f<ms / minute>

    [<Literal>]
    let LOWEST_SUPPORTED_RATE = 0.5f<rate>

    [<Literal>]
    let HIGHEST_SUPPORTED_RATE = 3.0f<rate>

module Setting =
    open Percyqaz.Common.Setting

    let rate x =
        x |> bounded(LOWEST_SUPPORTED_RATE, HIGHEST_SUPPORTED_RATE) |> roundf_uom 2
