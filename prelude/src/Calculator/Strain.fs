namespace Prelude.Calculator

open Prelude
open Prelude.Charts

[<Struct>]
type RowStrain =
    {
        NotesV1: float32 array
        StrainV1Notes: float32 array
    }

[<Struct>]
type RowStrainV2 =
    {
        Strains: float32 array
        Left: float32 * float32
        Right: float32 * float32
    }

module Strain =

    let STRAIN_SCALE = 0.01626f
    let STRAIN_TIME_CAP = 200.0f<ms / rate>

    let strain_func (half_life: float32<ms / rate>) =
        let DECAY_RATE = log 0.5f / half_life

        fun (value: float32) (input: float32) (delta: GameplayTime) ->
            let decay = exp (DECAY_RATE * min STRAIN_TIME_CAP delta)
            let time_cap_decay = if delta > STRAIN_TIME_CAP then exp (DECAY_RATE * (delta - STRAIN_TIME_CAP)) else 1.0f
            let a = value * time_cap_decay
            let b = input * input * STRAIN_SCALE
            b - (b - a) * decay

    let strain_burst = strain_func 1575.0f<ms / rate>
    let strain_stamina = strain_func 60000.0f<ms / rate>

    /// Current system - Looking to be made obselete by `calculate_hand_strains`
    let calculate_finger_strains (rate: Rate, notes: TimeArray<NoteRow>) (note_difficulty: NoteDifficulty array array) : RowStrain array =
        let keys = notes.[0].Data.Length
        let last_note_in_column = Array.zeroCreate<Time> keys

        let strain_v1 = Array.zeroCreate<float32> keys

        seq {
            for i = 0 to notes.Length - 1 do
                let { Time = offset; Data = nr } = notes.[i]

                let notes_v1 = Array.zeroCreate<float32> keys
                let row_strain_v1 = Array.zeroCreate<float32> keys

                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then

                        notes_v1.[k] <- note_difficulty.[i].[k].Total

                        strain_v1.[k] <-
                            strain_burst
                                strain_v1.[k]
                                notes_v1.[k]
                                ((offset - last_note_in_column.[k]) / rate)
                        row_strain_v1.[k] <- strain_v1.[k]

                        last_note_in_column.[k] <- offset

                yield
                    {
                        NotesV1 = notes_v1
                        StrainV1Notes = row_strain_v1
                    }
        }
        |> Array.ofSeq

    /// Upcoming system - NOT USED IN THE FINAL RATING RIGHT NOW
    /// May be a better model of strain to replace `calculate_finger_strain`
    let calculate_hand_strains (rate: Rate, notes: TimeArray<NoteRow>) (note_difficulty: NoteDifficulty array array) : RowStrainV2 array =
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

                        let d = note_difficulty.[i].[k].Total

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