namespace Prelude.Calculator.Patterns

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Calculator

[<RequireQualifiedAccess>]
type Direction =
    | None
    | Left
    | Right
    | Outwards
    | Inwards

type RowInfo =
    {
        Index: int
        Time: Time
        MsPerBeat: float32<ms / beat>
        Notes: int
        Jacks: int
        Direction: Direction
        Roll: bool
        Density: Density
        Variety: float32
        Strains: float32 array
        // todo: some kind of LN coverage metric
        RawNotes: int array
    }

module Primitives =

    let detect_direction (previous_row: int array) (current_row: int array) : Direction * bool =
        assert (previous_row.Length > 0)
        assert (current_row.Length > 0)

        let pleftmost = Array.head previous_row
        let prightmost = Array.last previous_row
        let cleftmost = Array.head current_row
        let crightmost = Array.last current_row

        let leftmost_change = cleftmost - pleftmost
        let rightmost_change = crightmost - prightmost

        let direction =
            if leftmost_change > 0 then
                if rightmost_change > 0 then
                    Direction.Right
                else
                    Direction.Inwards
            elif leftmost_change < 0 then
                if rightmost_change < 0 then
                    Direction.Left
                else
                    Direction.Outwards
            else
                if rightmost_change < 0 then
                    Direction.Inwards
                elif rightmost_change > 0 then
                    Direction.Outwards
                else
                    Direction.None
        let is_roll = pleftmost > crightmost || prightmost < cleftmost
        direction, is_roll

    let calculate (density: Density array, difficulty_info: Difficulty, chart: Chart) : RowInfo list =

        let { Time = first_note; Data = row } = (TimeArray.first chart.Notes).Value

        let mutable previous_row =
            seq { 0 .. chart.Keys - 1 }
            |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
            |> Array.ofSeq

        if previous_row.Length = 0 then
            Logging.Error("First row of chart is empty, wtf?")
            []
        else

        let mutable previous_time = first_note
        let mutable index = 0

        seq {
            for { Time = t; Data = row } in (chart.Notes |> Seq.skip 1) do
                index <- index + 1

                let current_row =
                    seq { 0 .. chart.Keys - 1 }
                    |> Seq.filter (fun k -> row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD)
                    |> Array.ofSeq

                if current_row.Length > 0 then

                    let direction, is_roll = detect_direction previous_row current_row

                    yield
                        {
                            Index = index
                            Time = (t - first_note)
                            MsPerBeat = (t - previous_time) * 4.0f< / beat>
                            Notes = current_row.Length
                            Jacks = current_row.Length - (Array.except previous_row current_row).Length
                            Direction = direction
                            Roll = is_roll
                            Density = density.[index]
                            Variety = difficulty_info.Variety.[index]
                            Strains = difficulty_info.Strains.[index].StrainV1Notes
                            RawNotes = current_row
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