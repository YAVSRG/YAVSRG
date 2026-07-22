namespace Prelude.Gameplay.Replays

open Prelude
open Prelude.Charts

type Autoplay =

    static let create_replay_internal (note_data: NoteData) : Replay =
        
        let first_note = note_data.Notes.[0].Time
        
        let inline time_until_next_movement (current_index: int) : Time =
            if current_index + 1 >= note_data.Notes.Length then 100.0f<ms>
            else
                let next_row = note_data.Notes.[current_index + 1]
                let this_row = note_data.Notes.[current_index]
                next_row.Time - this_row.Time

        let inline generate_autoplay_inputs() =
            seq {
                let mutable index = 0
                let mutable keys_held_down: Bitmask = Bitmask.Empty

                while index < note_data.Notes.Length do
                    let mutable keys_down_this_frame = keys_held_down
                    
                    let inline autoplay_note(key: int, note_type: NoteType) =
                        if note_type = NoteType.NORMAL then
                            keys_down_this_frame <- keys_down_this_frame.Add(key)
                            
                        elif note_type = NoteType.HOLDHEAD then
                            keys_down_this_frame <- keys_down_this_frame.Add(key)
                            keys_held_down <- keys_held_down.Add(key)
                            
                        elif note_type = NoteType.HOLDTAIL then
                            keys_down_this_frame <- keys_down_this_frame.Remove(key)
                            keys_held_down <- keys_held_down.Remove(key)

                    let note_row = note_data.Notes.[index]
                    for k = 0 to note_data.Keys - 1 do
                        autoplay_note(k, note_row.Data.[k])

                    yield ReplayFrame.Create(note_row.Time - first_note, keys_down_this_frame)
                    let halfway_to_next_movement = time_until_next_movement(index) * 0.5f
                    yield ReplayFrame.Create(note_row.Time - first_note + halfway_to_next_movement, keys_held_down)
                    index <- index + 1
            }
        
        { Frames = Array.ofSeq(generate_autoplay_inputs()) }

    static member FromNoteData : NoteData -> Replay = cached(create_replay_internal)
    
    static member inline CreateReplay<^T when ^T : (member ToNoteData : unit -> NoteData)>(chart: ^T) : Replay =
        Autoplay.FromNoteData(chart.ToNoteData())