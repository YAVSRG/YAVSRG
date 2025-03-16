namespace Prelude.Skins.Noteskins

open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Mods

// This is the final stage of preprocessing chart data before it is played by the user.
// Colorings are an assignment of a color id for each note. These ids are then used by skins to display differences in textures
// Some players may find certain coloring systems useful, for example having color coding depending on the musical beat a note is snapped to
// It is also common just to have simple column color variation to make columns appear distinct

type ColorScheme =
    | Column = 0
    | Chord = 1
    | DDR = 2

module ColorScheme =

    let DDR_VALUES =
        [| 1; 2; 3; 4; 6; 8; 12; 16 |]
        |> Array.map (fun i -> 48 / i)

    let count (keycount: int) (scheme: ColorScheme) : int =
        match scheme with
        | ColorScheme.Column -> keycount
        | ColorScheme.Chord -> keycount
        | ColorScheme.DDR -> DDR_VALUES.Length + 1
        | _ -> keycount

type ColorData = byte array

type ColoredChart =
    {
        Source: ModdedChart
        Colors: TimeArray<ColorData>
    }
    member this.Keys = this.Source.Keys
    member this.BPM = this.Source.BPM
    member this.SV = this.Source.SV
    member this.ModsSelected = this.Source.ModsSelected
    member this.ModsApplied = this.Source.ModsApplied
    member this.FirstNote = this.Source.FirstNote
    member this.LastNote = this.Source.LastNote
    member this.Notes = this.Source.Notes

[<Json.AutoCodec(false)>]
type ColorConfig =
    {
        Style: ColorScheme
        Colors: ColorData array
        UseGlobalColors: bool
    }
    static member Default =
        {
            Style = ColorScheme.Column
            Colors = Array.init 9 (fun i -> Array.init 10 byte)
            UseGlobalColors = true
        }

    member this.Validate =
        { this with
            Colors =
                if
                    Array.forall (fun (x: ColorData) -> x.Length = 10) this.Colors
                    && this.Colors.Length = 9
                then
                    this.Colors
                else
                    Logging.Error(
                        "Problem with noteskin: Colors should be an 9x10 array - Please use the ingame editor"
                    )

                    ColorConfig.Default.Colors
        }

module NoteColors =

    let private ddr_func (delta: Time) (ms_per_beat: float32<ms / beat>) : int =
        let tick = (float32 delta % float32 ms_per_beat) * 48.0f / float32 ms_per_beat |> round |> int

        let rec f i =
            if i >= ColorScheme.DDR_VALUES.Length then ColorScheme.DDR_VALUES.Length
            elif tick % ColorScheme.DDR_VALUES.[i] = 0 then i
            else f (i + 1)

        f 0

    let private column_colors (color_data: ColorData) (mc: ModdedChart) : TimeArray<ColorData> =

        let c = [| for i in 0 .. (mc.Keys - 1) -> color_data.[i] |]
        mc.Notes |> TimeArray.map (fun _ -> c)

    let private chord_colors (color_data: ColorData) (mc: ModdedChart) : TimeArray<ColorData> =

        let mutable previous_colors: ColorData = Array.zeroCreate mc.Keys

        mc.Notes
        |> TimeArray.map (fun nr ->

            let mutable index = -1

            for k = 0 to mc.Keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    index <- index + 1

            index <- max 0 index

            let colors = Array.create mc.Keys color_data.[index]

            for k = 0 to mc.Keys - 1 do
                if nr.[k] = NoteType.HOLDBODY || nr.[k] = NoteType.HOLDTAIL then
                    colors.[k] <- previous_colors.[k]
                else
                    previous_colors.[k] <- color_data.[index]

            colors
        )

    let private ddr_colors (color_data: ColorData) (mc: ModdedChart) : TimeArray<ColorData> =

        let mutable previous_colors: ColorData = Array.zeroCreate mc.Keys

        let mutable bpm_index = 0
        let mutable bpm_time = if mc.BPM.Length = 0 then 0.0f<ms> else mc.BPM.[0].Time

        let mutable bpm_mspb =
            if mc.BPM.Length = 0 then
                500.0f<ms / beat>
            else
                mc.BPM.[0].Data.MsPerBeat

        mc.Notes
        |> Array.map (fun { Time = time; Data = nr } ->

            while bpm_index < mc.BPM.Length - 1 && mc.BPM.[bpm_index + 1].Time < time do
                bpm_index <- bpm_index + 1
                bpm_time <- mc.BPM.[bpm_index].Time
                bpm_mspb <- mc.BPM.[bpm_index].Data.MsPerBeat

            let ddr_color = ddr_func (time - bpm_time) bpm_mspb

            let colors = Array.create mc.Keys color_data.[ddr_color]

            for k = 0 to mc.Keys - 1 do
                if nr.[k] = NoteType.HOLDBODY || nr.[k] = NoteType.HOLDTAIL then
                    colors.[k] <- previous_colors.[k]
                else
                    previous_colors.[k] <- color_data.[ddr_color]

            { Time = time; Data = colors }
        )

    let private apply_scheme (scheme: ColorScheme) (color_data: ColorData) (mc: ModdedChart) : ColoredChart =
        let colored_notes =
            match scheme with
            | ColorScheme.Column -> column_colors color_data mc
            | ColorScheme.Chord -> chord_colors color_data mc
            | ColorScheme.DDR -> ddr_colors color_data mc
            | _ -> column_colors (Array.zeroCreate mc.Keys) mc

        { Source = mc; Colors = colored_notes }

    let apply (config: ColorConfig) (chart: ModdedChart) : ColoredChart =
        let index = if config.UseGlobalColors then 0 else chart.Keys - 2
        apply_scheme config.Style config.Colors.[index] chart