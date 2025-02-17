namespace Prelude.Calculator.Patterns

open Percyqaz.Common
open Prelude
open Prelude.Charts

[<RequireQualifiedAccess>]
type Direction =
    | None
    | Left
    | Right
    | Outwards
    | Inwards

type Density = float32</rate>

type RowInfo =
    {
        RawNotes: int array
        Notes: int
        Jacks: int
        Direction: Direction
        Roll: bool
        Time: Time
        MsPerBeat: float32<ms / beat>
        Density: Density
    }

module Density =

    let private DENSITY_SENSITIVITY = 0.9f

    let private note (time: Time) (d: Density) =
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

module Primitives =

    // todo: LN coverage calculation

    let process_chart (chart: Chart) : Density array * RowInfo list =

        let { Time = first_note; Data = row } = (TimeArray.first chart.Notes).Value
        let density = Density.process_chart chart

        let mutable previous_row =
            seq { 0 .. chart.Keys - 1 }
            |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
            |> Array.ofSeq

        if previous_row.Length = 0 then
            Logging.Error("First row of chart is empty, wtf?")
            density, []
        else

        let mutable previous_time = first_note

        let mutable index = 0

        density,
        seq {
            for { Time = t; Data = row } in (chart.Notes |> Seq.skip 1) do
                index <- index + 1

                let current_row =
                    seq { 0 .. chart.Keys - 1 }
                    |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
                    |> Array.ofSeq

                if current_row.Length > 0 then

                    let pmin = Array.min previous_row
                    let pmax = Array.max previous_row
                    let cmin = Array.min current_row
                    let cmax = Array.max current_row

                    yield
                        {
                            RawNotes = current_row
                            Notes = current_row.Length
                            Jacks = current_row.Length - (Array.except previous_row current_row).Length
                            Direction =
                                let lo = cmin - pmin
                                let hi = cmax - pmax

                                if lo > 0 then
                                    if hi > 0 then Direction.Right else Direction.Inwards
                                elif lo < 0 then
                                    if hi < 0 then Direction.Left else Direction.Outwards
                                else if hi < 0 then
                                    Direction.Inwards
                                elif hi > 0 then
                                    Direction.Outwards
                                else
                                    Direction.None
                            Roll = pmin > cmax || pmax < cmin
                            Time = (t - first_note)
                            MsPerBeat = (t - previous_time) * 4.0f< / beat>
                            Density = density.[index]
                        }

                    previous_row <- current_row
                    previous_time <- t

        }
        |> List.ofSeq

module Metrics =

    let ln_percent (chart: Chart) : float32 =
        let mutable notes = 0
        let mutable lnotes = 0

        for { Data = nr } in chart.Notes do
            for n in nr do
                if n = NoteType.NORMAL then
                    notes <- notes + 1
                elif n = NoteType.HOLDHEAD then
                    notes <- notes + 1
                    lnotes <- lnotes + 1

        float32 lnotes / float32 notes

    let sv_time (chart: Chart) : Time =
        if chart.SV.Length = 0 then
            0.0f<ms>
        else

            let mutable total = 0.0f<ms>

            let mutable time = chart.FirstNote
            let mutable vel = 1.0f

            for sv in chart.SV do
                if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                    total <- total + (sv.Time - time)

                vel <- sv.Data
                time <- sv.Time

            if not (System.Single.IsFinite vel) || abs (vel - 1.0f) > 0.01f then
                total <- total + (chart.LastNote - time)

            total