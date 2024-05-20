namespace Prelude.Charts.Processing.Patterns

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

[<Measure>]
type rate

type ScaledTime = float32<ms / rate>

type RowInfo =
    {
        RawNotes: int array
        Notes: int
        Jacks: int
        Direction: Direction
        Roll: bool
        Time: float32<ms / rate>
        MsPerBeat: float32<ms / beat>
        Density: float32
    }

module Analysis =

    let private DENSITY_SENTITIVITY = -4f

    let private density_step (time: Time) (d: float32) =
        let seconds = time / 1000f<ms>
        d * System.MathF.Exp(DENSITY_SENTITIVITY * seconds)

    let private density_hit (time: Time) (d: float32) =
        let seconds = time / 1000f<ms>

        density_step time d
        + (1.0f - System.MathF.Exp(DENSITY_SENTITIVITY * seconds)) / seconds

    let density_data (rate: float32) (chart: Chart) =
        let column_densities = Array.zeroCreate chart.Keys
        let column_sinces = Array.create chart.Keys -Time.infinity

        seq {
            for { Time = t; Data = row } in chart.Notes do
                for k = 0 to chart.Keys - 1 do
                    if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                        column_densities.[k] <- density_hit ((t - column_sinces.[k]) / rate) column_densities.[k]
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
        : int array * int array =
        let mutable i = 0
        let mutable notes = 0
        let mutable rows = 0
        let mutable last_sample = 0
        let mutable last_sample_rows = 0
        let start = chart.FirstNote
        let length = chart.LastNote - start
        let interval = length / float32 samples

        let notecounts = Array.zeroCreate samples
        let rowcounts = Array.zeroCreate samples

        if length > 0.0f<ms> then

            for { Time = t; Data = row } in chart.Notes do
                let mutable is_empty = true

                for nt in row do
                    if nt = NoteType.NORMAL || nt = NoteType.HOLDHEAD then
                        is_empty <- false
                        notes <- notes + 1

                if not is_empty then
                    rows <- rows + 1

                while t - start >= interval * float32 (i + 1) do
                    notecounts.[i] <- notes - last_sample
                    rowcounts.[i] <- rows - last_sample_rows
                    last_sample <- notes
                    last_sample_rows <- rows
                    i <- i + 1

            if i <> samples then
                notecounts.[samples - 1] <- notes - last_sample
                rowcounts.[samples - 1] <- rows - last_sample_rows

        notecounts, rowcounts

    let run (rate: float32) (chart: Chart) : RowInfo list =

        let { Time = first_note; Data = row } = (TimeArray.first chart.Notes).Value
        let density = density_data rate chart

        let mutable previous_row =
            seq { 0 .. chart.Keys - 1 }
            |> Seq.filter (fun x -> row.[x] = NoteType.NORMAL || row.[x] = NoteType.HOLDHEAD)
            |> Array.ofSeq

        if previous_row.Length = 0 then
            Logging.Error(sprintf "First row of chart is empty, wtf?\n%A" chart.Header)
            []
        else

        let mutable previous_time = first_note

        let mutable index = 0

        seq {
            for { Time = t; Data = row } in (chart.Notes |> Seq.skip 1) do
                index <- index + 1

                let current_row =
                    seq { 0 .. chart.Keys - 1 }
                    |> Seq.filter (fun x -> row.[x] = NoteType.NORMAL || row.[x] = NoteType.HOLDHEAD)
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
                            Time = (t - first_note) / (rate * 1.0f<rate>)
                            MsPerBeat = (t - previous_time) * 4.0f< / beat> / rate
                            Density = density.[index]
                        }

                    previous_row <- current_row
                    previous_time <- t

        }
        |> List.ofSeq
