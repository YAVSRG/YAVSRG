namespace Prelude.Gameplay.Replays

open System
open Prelude
open Prelude.Charts
        
type WavyAutoplay =

    static let create_replay_internal (note_data: NoteData) : Replay =
        let inline wavy_offset (time: Time) = MathF.Cos(time / 1000.0f<ms>) * 45.0f<ms>
        
        let mutable last_time = -Time.infinity
        
        let inline apply_offset (frame: ReplayFrame) : ReplayFrame =
            let offset_applied = frame.Time + wavy_offset(frame.Time)
            let timestamp_ensure_nondecreasing = max last_time offset_applied
            last_time <- timestamp_ensure_nondecreasing
            ReplayFrame.Create(timestamp_ensure_nondecreasing, frame.PressedKeys)

        let autoplay = Autoplay.CreateReplay(note_data)
        { Frames = autoplay.Frames |> Array.map apply_offset }

    static member FromNoteData : NoteData -> Replay = cached(create_replay_internal)
    
    /// Generates a replay for provided notes where hits are slightly offset based on a sine wave over time
    /// Used in Interlude's HUD editor so you can see what varying hit ms deviations look like in various HUD elements
    static member inline CreateReplay<^T when ^T : (member ToNoteData : unit -> NoteData)>(chart: ^T) =
        WavyAutoplay.FromNoteData(chart.ToNoteData())