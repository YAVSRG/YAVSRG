namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts

module Layout =

    let keys_on_left_hand (keymode: int) =
        match keymode with
        | 3 -> 2
        | 4 -> 2
        | 5 -> 3
        | 6 -> 3
        | 7 -> 4
        | 8 -> 4
        | 9 -> 5
        | 10 -> 5
        | _ -> failwithf "Invalid keymode %i" keymode

[<Struct>]
type NoteDifficulty =
    {
        mutable J: float32
        mutable SL: float32
        mutable SR: float32
    }

[<Struct>]
type RowStrain =
    {
        NotesV0: float32 array
        NotesV1: float32 array
        StrainV0: float32
        StrainV0Notes: float32 array
        StrainV1Notes: float32 array
    }

[<Struct>]
type RowStrainV2 =
    {
        Strains: float32 array
        Left: float32 * float32
        Right: float32 * float32
    }

type Difficulty =
    {
        NoteDifficulty: NoteDifficulty array array
        Strains: RowStrain array
        Hands: RowStrainV2 array
        Overall: float
    }

module Difficulty =

    /// Cutoff of `JACK_CURVE_CUTOFF` prevents stacked minijacks from slingshotting the perceived BPM
    /// Especially since you can typically hit such minijacks with good accuracy by playing them as a 230 jack no matter the BPM
    let JACK_CURVE_CUTOFF = 230.0f
    /// Converts ms difference between notes (in the same column) into its equivalent BPM
    /// (on 1/4 snap, hence 1 minute * 1/4 is numerator)
    let ms_to_jack_bpm (delta: GameplayTime) =
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
    let ms_to_stream_bpm (delta: GameplayTime) =
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
    let jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) =
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
    let private notes_difficulty_pass (rate: Rate, notes: TimeArray<NoteRow>) (data: NoteDifficulty array array) =
        let keys = notes.[0].Data.Length
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

    let private OHTNERF = 3.0f
    let STREAM_CURVE_HEIGHT_SCALE = 0.5209125f
    let note_strain_v0 (note: NoteDifficulty) : float32 =
        MathF.Pow(
            MathF.Pow(STREAM_CURVE_HEIGHT_SCALE * note.SL, OHTNERF) +
            MathF.Pow(STREAM_CURVE_HEIGHT_SCALE * note.SR, OHTNERF) +
            MathF.Pow(note.J, OHTNERF),
            1.0f / OHTNERF
        )

    let STREAM_SCALE = 6f
    let STREAM_POW = 0.5f
    let note_strain_v1 (note: NoteDifficulty) : float32 =
        MathF.Pow(
            MathF.Pow(STREAM_SCALE * note.SL ** STREAM_POW, OHTNERF) +
            MathF.Pow(STREAM_SCALE * note.SR ** STREAM_POW, OHTNERF) +
            MathF.Pow(note.J, OHTNERF),
            1.0f / OHTNERF
        )

    let stamina_func (value: float32) (input: float32) (delta: GameplayTime) =

        let decay = exp (-0.00044f<rate / ms> * delta)
        let v = Math.Max(value * decay, 0.01f)
        let ratio = input / v
        let mult = 1.0f + 0.105f * ratio
        v * mult

    let STRAIN_SCALE = 0.01626f

    let STRAIN_BURST_HALF_LIFE = 1575.0f<ms / rate>
    let STRAIN_BURST_DECAY_RATE = log 0.5f / STRAIN_BURST_HALF_LIFE
    let strain_burst (value: float32) (input: float32) (delta: GameplayTime) =

        let decay = exp (STRAIN_BURST_DECAY_RATE * delta) |> max 0.5f
        let a = value
        let b = input * input * STRAIN_SCALE
        b - (b - a) * decay

    let STRAIN_STAMINA_HALF_LIFE = 60000.0f<ms / rate>
    let STRAIN_STAMINA_DECAY_RATE = log 0.5f / STRAIN_STAMINA_HALF_LIFE
    let strain_stamina (value: float32) (input: float32) (delta: GameplayTime) =

        let decay = exp (STRAIN_STAMINA_DECAY_RATE * delta) |> max 0.5f
        let a = value
        let b = input * input * STRAIN_SCALE
        b - (b - a) * decay

    let finger_strain_pass (rate: Rate, notes: TimeArray<NoteRow>) (note_difficulty: NoteDifficulty array array) : RowStrain array =
        let keys = notes.[0].Data.Length
        let last_note_in_column = Array.zeroCreate<Time> keys

        let strain_v0 = Array.zeroCreate<float32> keys
        let strain_v1 = Array.zeroCreate<float32> keys

        seq {
            for i = 0 to notes.Length - 1 do
                let { Time = offset; Data = nr } = notes.[i]

                let notes_v0 = Array.zeroCreate<float32> keys
                let notes_v1 = Array.zeroCreate<float32> keys
                let row_strain_v0 = Array.zeroCreate<float32> keys
                let row_strain_v1 = Array.zeroCreate<float32> keys

                let mutable sum = 0.0f
                let mutable n = 0.0f
                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then

                        notes_v0.[k] <- note_strain_v0 note_difficulty.[i].[k]
                        notes_v1.[k] <- note_strain_v1 note_difficulty.[i].[k]

                        strain_v0.[k] <-
                            stamina_func
                                strain_v0.[k]
                                notes_v0.[k]
                                ((offset - last_note_in_column.[k]) / rate)
                        row_strain_v0.[k] <- strain_v0.[k]

                        strain_v1.[k] <-
                            strain_burst
                                strain_v1.[k]
                                notes_v1.[k]
                                ((offset - last_note_in_column.[k]) / rate)
                        row_strain_v1.[k] <- strain_v1.[k]

                        last_note_in_column.[k] <- offset

                        sum <- sum + strain_v0.[k]
                        n <- n + 1.0f

                yield
                    {
                        NotesV0 = notes_v0
                        NotesV1 = notes_v1
                        StrainV0 = if n = 0.0f then 0.0f else sum / n
                        StrainV0Notes = row_strain_v0
                        StrainV1Notes = row_strain_v1
                    }
        }
        |> Array.ofSeq

    let hand_strain_pass (rate: Rate, notes: TimeArray<NoteRow>) (note_difficulty: NoteDifficulty array array) : RowStrainV2 array =
        let keys = notes.[0].Data.Length
        let hand_split = Layout.keys_on_left_hand keys

        let last_note_in_column = Array.init<_> keys (fun _ -> 0.0f, 0.0f, 0.0f<ms>)

        seq {
            for i = 0 to notes.Length - 1 do
                let { Time = offset; Data = nr } = notes.[i]

                let mutable left_hand_burst = 0.00f
                let mutable left_hand_stamina = 0.00f

                let mutable right_hand_burst = 0.00f
                let mutable right_hand_stamina = 0.00f

                let strain = Array.zeroCreate<float32> keys

                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then

                        let d = note_strain_v1 note_difficulty.[i].[k]

                        if k < hand_split then
                            for hand_k = 0 to hand_split - 1 do
                                let pburst, pstamina, ptime = last_note_in_column.[hand_k]
                                left_hand_burst <- max left_hand_burst (strain_burst pburst d ((offset - ptime) / rate))
                                left_hand_stamina <- max left_hand_stamina (strain_stamina pstamina d ((offset - ptime) / rate))
                        else
                            for hand_k = hand_split to keys - 1 do
                                let pburst, pstamina, ptime = last_note_in_column.[hand_k]
                                right_hand_burst <- max right_hand_burst (strain_burst pburst d ((offset - ptime) / rate))
                                right_hand_stamina <- max right_hand_stamina (strain_stamina pstamina d ((offset - ptime) / rate))

                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                        if k < hand_split then
                            last_note_in_column.[k] <- left_hand_burst, left_hand_stamina, offset
                            strain.[k] <- left_hand_burst * 0.875f + left_hand_stamina * 0.125f
                        else
                            last_note_in_column.[k] <- right_hand_burst, right_hand_stamina, offset
                            strain.[k] <- right_hand_burst * 0.875f + right_hand_stamina * 0.125f

                yield { Strains = strain; Left = left_hand_burst, left_hand_stamina; Right = right_hand_burst, right_hand_stamina }
        }
        |> Array.ofSeq

    let CURVE_POWER = 0.6f
    let CURVE_SCALE = 0.4056f
    /// Calculates a single number to represent a set of difficulty data points
    /// This number represents how a player would "perceive" the difficulty overall
    /// Perception is not a concrete term so I've ruled: In my experience players perception is concentrated on the very hardest parts of a chart
    let weighted_overall_difficulty (data: float32 seq) : float32 =

        /// The top 2500 notes get special weightings
        let MOST_IMPORTANT_NOTES = 2500.0f
        /// Weightings for the values are calculated by this formula
        /// x = % position through the top 2500 from 0.0 - 1.0
        /// Values below the top 2500 always have x = 0.0
        let WEIGHTING_CURVE x = 0.002f + x ** 4.0f

        let data_array = data |> Seq.filter (fun x -> x > 0.0f) |> Seq.sort |> Array.ofSeq
        let length = float32 data_array.Length

        let mutable weight = 0.0f
        let mutable total = 0.0f

        for i = 0 to data_array.Length - 1 do
            let w = WEIGHTING_CURVE ((float32 i + MOST_IMPORTANT_NOTES - length) / MOST_IMPORTANT_NOTES |> max 0.0f)
            weight <- weight + w
            total <- total + data_array.[i] * w

        // Final transform on the weighted average: Power and rescale it to some arbitrary scale people like and are used to
        MathF.Pow(total / weight, CURVE_POWER) * CURVE_SCALE

    let private calculate_uncached (rate: Rate, notes: TimeArray<NoteRow>) : Difficulty =
        let keys = notes.[0].Data.Length
        let note_data = Array.init notes.Length (fun _ -> Array.zeroCreate keys)
        notes_difficulty_pass (rate, notes) note_data
        let physical_data = finger_strain_pass (rate, notes) note_data
        let hands = hand_strain_pass (rate, notes) note_data
        let physical = weighted_overall_difficulty (physical_data |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f))

        {
            NoteDifficulty = note_data
            Strains = physical_data
            Hands = hands
            Overall = if Single.IsFinite physical then float physical else 0.0
        }

    let calculate = calculate_uncached |> cached

    let color v =
        try
            let a = v * 0.1 |> min 1.0
            let b = (v * 0.1 |> max 1.0) - 1.0 |> min 1.0
            Color.FromArgb(255.0 * a |> int, 255.0 * (1.0 - a) |> int, 255.0 * b |> int)
        with _ -> Color.Red