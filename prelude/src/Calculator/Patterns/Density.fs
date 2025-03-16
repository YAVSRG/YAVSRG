namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts

type Density = float32</rate>

module Density =

    let private DENSITY_SENSITIVITY = 0.9f

    let private note (time: Time) (d: Density) : Density =
        let next_d = 1000.0f<ms / rate> / time
        d * DENSITY_SENSITIVITY + next_d * (1.0f - DENSITY_SENSITIVITY)

    let process_chart (chart: Chart) : Density array =
        let column_densities = Array.zeroCreate chart.Keys
        let column_sinces = Array.create chart.Keys -Time.infinity

        seq {
            for { Time = t; Data = row } in chart.Notes do
                for k = 0 to chart.Keys - 1 do
                    if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                        column_densities.[k] <- note ((t - column_sinces.[k])) column_densities.[k]
                        column_sinces.[k] <- t

                yield Array.max column_densities
        }
        |> Array.ofSeq

    let inline nps_cps
        (samples: int)
        (chart:
            ^T
                when ^T: (member FirstNote: Time)
                and ^T: (member LastNote: Time)
                and ^T: (member Notes: TimeArray<NoteRow>))
        : Density array * Density array =
        let start = chart.FirstNote
        let duration = chart.LastNote - start

        let interval = duration / float32 samples

        let notecounts = Array.zeroCreate samples
        let rowcounts = Array.zeroCreate samples

        if duration > 0.0f<ms> then
            for { Time = t; Data = row } in chart.Notes do

                let s = float32 samples * (t - start) / duration |> int |> max 0 |> min (samples - 1)

                let mutable is_empty = true
                for nt in row do
                    if nt = NoteType.NORMAL || nt = NoteType.HOLDHEAD then
                        is_empty <- false
                        notecounts.[s] <- notecounts.[s] + 1

                if not is_empty then
                    rowcounts.[s] <- rowcounts.[s] + 1

        notecounts |> Array.map (fun i -> float32 i / interval * 1000f<ms / rate>),
        rowcounts |> Array.map (fun i -> float32 i / interval * 1000f<ms / rate>)