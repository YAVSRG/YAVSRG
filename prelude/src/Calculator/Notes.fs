namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts

[<Struct>]
type NoteDifficulty =
    {
        mutable J: float32
        mutable SL: float32
        mutable SR: float32
    }

module NoteDifficulty =

    /// Cutoff of `JACK_CURVE_CUTOFF` prevents stacked minijacks from slingshotting the perceived BPM
    /// Especially since you can typically hit such minijacks with good accuracy by playing them as a 230 jack no matter the BPM
    let JACK_CURVE_CUTOFF = 230.0f
    /// Converts ms difference between notes (in the same column) into its equivalent BPM
    /// (on 1/4 snap, hence 1 minute * 1/4 is numerator)
    let ms_to_jack_bpm (delta: GameplayTime) : float32 =
        15000.0f<ms / rate> / delta
        |> min JACK_CURVE_CUTOFF

    /// These variables adjust what BPM the stream curve cuts off at
    /// Best to put this whole expression into https://www.desmos.com/calculator + variables as sliders if you want to understand it better
    let STREAM_CURVE_CUTOFF = 10.0f
    let STREAM_CURVE_CUTOFF_2 = 10.0f
    /// Converts ms difference between notes (in the same column) into its equivalent BPM (on 1/4 snap)
    ///
    /// Once the ms gets low enough, the curve starts going back down towards 0
    /// since adjacent notes that would make a "500bpm stream" can be hit as grace notes and don't add difficulty
    /// The real threshold is lower than 500 though, and controlled by the cutoff variables
    let ms_to_stream_bpm (delta: GameplayTime) : float32 =
        300.0f / (0.02f<rate / ms> * delta) -
        300.0f / MathF.Pow(0.02f<rate / ms> * delta, STREAM_CURVE_CUTOFF) / STREAM_CURVE_CUTOFF_2
        |> max 0.0f

    /// Consider note A in column 1, note B also in column 1, 100ms earlier and note C in column 2, also 100ms earlier
    /// Note A should not get any "stream" value because you will be using your wrist to tap the key again, right after note B
    ///
    /// This function uses the ratio between the jack and stream spacing to determine a multiplier between 0.0 and 1.0
    /// Example: for [12][1] as described above, 0.0 is returned to fully cancel out the stream value
    /// Example: for [1][2][1] evenly spaced trill (note C is 50ms earlier instead), 1.0 is returned to do no cancelling at all
    /// Best to put this whole expression into https://www.desmos.com/calculator if you want to understand it better
    let jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) : float32 =
        let ratio = jack_delta / stream_delta
        MathF.Log(ratio, 2.0f)
        |> max 0.0f
        |> sqrt
        |> min 1.0f

    /// Walks forward through each row of notes in a chart:
    /// - Sets J ("jack") on each note to the BPM between it and the previous note in its column
    /// - Sets SL ("stream-left") on each note to the BPM betwen it and the previous note in the column to the left, if this column exists and is on the same hand
    /// - Sets SR ("stream-right") on each note to the BPM betwen it and the previous note in the column to the right, if this column exists and is on the same hand
    /// For higher keymodes, SL and SR get the maximum value out of all left- and right-notes respectively
    let calculate_note_ratings (rate: Rate, notes: TimeArray<NoteRow>) : NoteDifficulty array array =
        let keys = notes.[0].Data.Length
        let data = Array.init notes.Length (fun _ -> Array.zeroCreate keys)
        let hand_split = Layout.keys_on_left_hand keys

        let last_note_in_column = Array.create<Time> keys ((TimeArray.first notes).Value.Time - 1000000.0f<ms>)

        let note_difficulty (i: int, k: int, time: Time) =
            let jack_delta =
                let delta = (time - last_note_in_column.[k]) / rate
                data.[i].[k].J <- ms_to_jack_bpm delta
                delta

            let hand_lo, hand_hi =
                if k < hand_split then
                    0, hand_split - 1
                else
                    hand_split, keys - 1

            let mutable sl = 0.0f
            let mutable sr = 0.0f
            for hand_k = hand_lo to hand_hi do
                if hand_k <> k then
                    let trill_delta = (time - last_note_in_column.[hand_k]) / rate
                    let trill_v = ms_to_stream_bpm trill_delta * jack_compensation jack_delta trill_delta
                    if hand_k < k then
                        sl <- max sl trill_v
                    else
                        sr <- max sr trill_v

            data.[i].[k].SL <- sl
            data.[i].[k].SR <- sr

        for i = 0 to notes.Length - 1 do
            let { Time = time; Data = nr } = notes.[i]

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    note_difficulty (i, k, time)

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    last_note_in_column.[k] <- time

        data

    let OHTNERF = 3.0f
    let STREAM_SCALE = 6f
    let STREAM_POW = 0.5f
    /// Combines all the parts of a note found by `calculate_note_ratings` and creates a single "this note is a bit like X bpm jacks" number
    let total (note: NoteDifficulty) : float32 =
        MathF.Pow(
            MathF.Pow(STREAM_SCALE * note.SL ** STREAM_POW, OHTNERF) +
            MathF.Pow(STREAM_SCALE * note.SR ** STREAM_POW, OHTNERF) +
            MathF.Pow(note.J, OHTNERF),
            1.0f / OHTNERF
        )

type NoteDifficulty with
    member this.Total = NoteDifficulty.total this