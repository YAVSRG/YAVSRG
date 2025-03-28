namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts

type Difficulty =
    {
        NoteDifficulty: NoteDifficulty array array
        Strains: RowStrain array
        Variety: float32 array
        Hands: RowStrainV2 array
        Overall: float32
    }

module Difficulty =

    let CURVE_POWER = 0.6f
    let CURVE_SCALE = 0.4056f
    /// The top 2500 notes get special weightings
    let MOST_IMPORTANT_NOTES = 2500.0f
    /// Weightings for the values are calculated by this formula
    /// x = % position through the top 2500 from 0.0 - 1.0
    /// Values below the top 2500 always have x = 0.0
    let WEIGHTING_CURVE x = 0.002f + x ** 4.0f

    /// Calculates a single number to represent a set of difficulty data points
    /// This number represents how a player would "perceive" the difficulty overall
    /// Perception is not a concrete term so I've ruled: In my experience players perception is concentrated on the very hardest parts of a chart
    let weighted_overall_difficulty (data: float32 seq) : float32 =

        let data_array = data |> Seq.sort |> Array.ofSeq
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
        let note_data = NoteDifficulty.calculate_note_ratings (rate, notes)
        let variety = Variety.calculate_variety (rate, notes) note_data
        let physical_data = Strain.calculate_finger_strains (rate, notes) note_data
        let hands = Strain.calculate_hand_strains (rate, notes) note_data
        let physical = weighted_overall_difficulty (physical_data |> Seq.map _.StrainV1Notes |> Seq.concat |> Seq.filter (fun x -> x > 0.0f))

        {
            NoteDifficulty = note_data
            Strains = physical_data
            Variety = variety
            Hands = hands
            Overall = if Single.IsFinite physical then physical else 0.0f
        }

    let calculate = calculate_uncached |> cached

    let color (rating: float32) : Color =
        let red = int(rating * 0.1f * 255.0f) |> min 255 |> max 0
        let green = 255 - red
        let blue = int(rating * 0.1f * 255.0f - 255.0f) |> min 255 |> max 0

        Color.FromArgb(red, green, blue)