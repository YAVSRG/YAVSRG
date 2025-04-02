namespace Prelude.Calculator

open Prelude
open Prelude.Charts

module HoldCoverage =

    let COVERAGE_WINDOW = 250.0f<ms / rate>
    let SHORTEN_AMOUNT = 80.0f<ms / rate>

    let calculate_coverage (keys: int, notes: TimeArray<NoteRow>, rate: Rate) : float32 array =

        let output = Array.zeroCreate notes.Length
        let holds_in_view = Array.zeroCreate keys
        let window = COVERAGE_WINDOW * rate
        let shorten = SHORTEN_AMOUNT * rate

        let mutable front_index = -1
        let mutable back_index = 0

        let mutable start = 0.0f<ms>
        let mutable in_hold = false

        for i = 0 to notes.Length - 1 do
            let time = notes.[i].Time

            while front_index + 1 < notes.Length && notes.[front_index + 1].Time < time + window + shorten do
                front_index <- front_index + 1
                for k = 0 to keys - 1 do
                    if notes.[front_index].Data.[k] = NoteType.HOLDHEAD then
                        holds_in_view.[k] <- holds_in_view.[k] + 1

            while back_index + 1 < notes.Length && notes.[back_index].Time < time - window do
                for k = 0 to keys - 1 do
                    if notes.[back_index].Data.[k] = NoteType.HOLDTAIL then
                        holds_in_view.[k] <- holds_in_view.[k] - 1
                        assert(holds_in_view.[k] >= 0)
                back_index <- back_index + 1

            for k = 0 to keys - 1 do

                if holds_in_view.[k] > 0 then

                    start <- time - window
                    in_hold <- false
                    for j = back_index to front_index do
                        if notes.[j].Data.[k] = NoteType.HOLDHEAD then
                            start <- max start notes.[j].Time |> min (time + window)
                            in_hold <- true
                        elif notes.[j].Data.[k] = NoteType.HOLDBODY then
                            output.[i] <- output.[i] + float32 (min (time + window) notes.[j].Time - start)
                            start <- max start notes.[j].Time |> min (time + window)
                            in_hold <- true
                        elif notes.[j].Data.[k] = NoteType.HOLDTAIL then
                            output.[i] <- output.[i] + float32 (notes.[j].Time - shorten - start)
                            in_hold <- false
                    if in_hold then
                        output.[i] <- output.[i] + float32 (time + window - start)

            output.[i] <- output.[i] / 2.0f / float32 window / float32 keys

        output