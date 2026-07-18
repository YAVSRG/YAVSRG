namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts

type NotesBuilder(keycount: int) =
    let items = ResizeArray<TimeItem<NoteRow>>()

    let mutable last_time = -Time.infinity

    member this.Note(time: Time, key: int) : NotesBuilder =

        if time > last_time then

            let row = Array.zeroCreate keycount
            row.[key] <- NoteType.NORMAL
            items.Add({ Time = time; Data = row })

            last_time <- time

        elif time = last_time then

            let row = items.[items.Count - 1].Data

            if row.[key] <> NoteType.NOTHING then
                failwithf "Stacked note at %f" time

            row.[key] <- NoteType.NORMAL

        else
            failwithf "Note timestamps went backwards: %f -> %f" last_time time

        this

    member this.Note(time: Time) : NotesBuilder = this.Note(time, 0)

    member this.HoldUntil(time: Time, until: Time, key: int) : NotesBuilder =

        if time > last_time then

            let head = Array.zeroCreate keycount
            head.[key] <- NoteType.HOLDHEAD
            items.Add({ Time = time; Data = head })

        elif time = last_time then

            let row = items.[items.Count - 1].Data

            if row.[key] <> NoteType.NOTHING then
                failwithf "Stacked hold at %f" time

            row.[key] <- NoteType.HOLDHEAD

        else
            failwithf "Hold timestamps went backwards: %f -> %f" last_time time

        let tail = Array.zeroCreate keycount
        tail.[key] <- NoteType.HOLDTAIL
        items.Add({ Time = until; Data = tail })

        last_time <- until

        this

    member this.HoldUntil(time: Time, until: Time) : NotesBuilder = this.HoldUntil(time, until, 0)

    member this.Build() : NoteData =
        { Keys = keycount; Notes = items.ToArray() }
