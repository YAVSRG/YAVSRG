namespace Prelude.Gameplay

open System
open Prelude.Charts.Interlude
open Prelude.Editor
open Prelude.Common

//This is the final stage of preprocessing chart data before it is played by the user.
//Colorings are an assignment of a color id for each note. These ids are then used by skins to display differences in textures
//Some players may find certain coloring systems useful, for example having color coding depending on the musical beat a note is snapped to
//It is also common just to have simple column color variation to make columns appear distinct

module NoteColors = 

    let DDRValues = [|1.0f; 2.0f; 3.0f; 4.0f; 6.0f; 8.0f; 12.0f; 16.0f|] |> Array.map (fun i -> i * 1.0f</beat>)

    type ColorScheme = 
        | Column
        | Chord
        | DDR
        | Jackhammer
        member this.ColorCount keycount =
            match this with
            | Column -> keycount
            | Chord -> keycount
            | DDR -> Array.length DDRValues
            | Jackhammer -> Array.length DDRValues

    type ColorData = byte array
    type ColorDataSets = ColorData array //color config per keymode. 0 stores "all keymode" data, 1 stores 3k, 2 stores 4k, etc
    type Colorizer<'state> = 'state -> TimeDataItem<NoteRow> -> ('state * ColorData)

    type private ColorNoteRow = (struct (NoteRow * ColorData))
    type ColorizedChart = int * TimeData<ColorNoteRow> * TimeData<BPM> * MultiTimeData<float32> * string list

    let colorize ((keys, notes, bpm, sv, m) : ModChart) (initialState, col : Colorizer<'t>) =
        let (_, _, data) = 
            Seq.fold (fun (lastcolors : ColorData, s, data : (TimeDataItem<ColorNoteRow>) list) (time, nr) ->
            (
                let (ns, d) = col s (time, nr)
                for k in getBits ((noteData NoteType.HOLDBODY nr) ||| (noteData NoteType.HOLDTAIL nr)) do
                    d.[k] <- lastcolors.[k]
                (d, ns, (time, (nr, d)) :: data)
            )) (Array.zeroCreate(keys), initialState, []) notes.Data
        in data
        |> Seq.rev
        |> ResizeArray<TimeDataItem<ColorNoteRow>>
        |> TimeData<ColorNoteRow>

    let private roughlyDivisible (a : Time) (b : Time) = Time.Abs(a - b * float32 (Math.Round(float <| a / b))) < 3.0f<ms>

    let private ddr_func (delta: Time) (msPerBeat : float32<ms/beat>) : int =
        List.tryFind ((fun i -> DDRValues.[i]) >> fun n -> roughlyDivisible delta (msPerBeat / n)) [0..7]
        |> Option.defaultValue DDRValues.Length

    let applyScheme (scheme : ColorScheme) (colorData : ColorData) (chart : ModChart) =
        let (keys, _, bpm, sv, m) = chart
        let ci i = colorData.[i]
        match scheme with
        | Column ->
            ((), fun _ (_, nr) ->
                ((), [|for i in 0..(keys-1) -> ci i|]))
                |> colorize chart
        | Chord ->
            ((), fun _ (_, nr) ->
                ((), Array.create keys ((nr |> noteData NoteType.NORMAL |> countBits) + (nr |> noteData NoteType.HOLDHEAD |> countBits) |> ci)))
                |> colorize chart
        | DDR ->
            (chart, fun c (time, nr) ->
                let (ptime, (_, msPerBeat)) =
                    if bpm.IsEmpty() then
                        (0.0f<ms>, (4<beat>, 500.0f<ms/beat>))
                    elif offsetOf <| bpm.First() >= time then bpm.First()
                    else
                        bpm.GetPointAt(time)
                in (chart, Array.create keys ((ddr_func (time - ptime) msPerBeat) |> ci)))
                |> colorize chart
        | _ ->
            ((), fun _ _ ->
                ((), Array.zeroCreate(keys)))
                |> colorize chart
        |> fun x -> (keys, x, bpm, sv, m)

    type ColorConfig = {
        Style: ColorScheme
        Colors: ColorDataSets
        UseGlobalColors: bool
    }
    with
        static member Default = { Style = ColorScheme.Column; Colors = Array.init 11 (fun i -> Array.init 10 byte); UseGlobalColors = true }

    let getColoredChart (config: ColorConfig) (chart: Lazy<ModChart>) =
        lazy (
            let chart = chart.Force()
            let index =
                if config.UseGlobalColors then 0
                else
                    let (keys, _, _, _, _) = chart in keys - 2
            applyScheme config.Style config.Colors.[index] chart )