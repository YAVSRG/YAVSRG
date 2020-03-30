module Prelude.Gameplay.Colors

open System
open Prelude.Charts.Interlude

//This is the final stage of preprocessing chart data before it is played by the user.
//Colorings are an assignment of a color id for each note. These ids are then used by skins to display differences in textures
//Some players may find certain coloring systems useful, for example having color coding depending on the musical beat a note is snapped to
//It is also common just to have simple column color variation to make columns appear distinct

let DDRValues = [|1; 2; 3; 4; 6; 8; 12; 16|]

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

type ColorData = byte array //always of size 10
type ColorDataSets = ColorData array //color config per keymode. 0 stores "all keymode" data, 1 stores 3k, 2 stores 4k, etc
type Colorizer<'state> = 'state -> TimeDataItem<NoteRow> -> ('state * ColorData)

let colorize (chart : Chart) (initialState, col : Colorizer<'t>) =
    (Seq.fold (fun (s, data : (TimeDataItem<NoteRow * ColorData>) list) (time, nr) -> ( let (ns, d) = col s (time, nr) in (ns, (time, (nr, d)) :: data) ))
        (initialState, []) chart.Notes.Enumerate
        |> snd)
    |> Seq.rev
    |> ResizeArray<TimeDataItem<NoteRow * ColorData>>
    |> TimeData<NoteRow * ColorData>

let private roughlyDivisible (a : float) (b : float) = Math.Abs(a - b * Math.Round(a / b)) < 3.0

let private ddr_func delta (msPerBeat : float) : int =
    List.tryFind ((fun i -> DDRValues.[i]) >> fun n -> roughlyDivisible delta (msPerBeat / (float n))) [0..7]
    |> Option.defaultValue DDRValues.Length

let applyColorizer (scheme : ColorScheme) (colorData : ColorData) (chart : Chart) =
    let ci i = colorData.[i]
    match scheme with
    | Column ->
        ((), fun _ (_, nr) ->
            ((), [|for i in 0..(chart.Keys-1) -> ci i|]))
            |> colorize chart
    | Chord ->
        ((), fun _ (_, nr) ->
            ((), Array.create chart.Keys ((nr |> noteData NoteType.NORMAL |> countBits) + (nr |> noteData NoteType.HOLDHEAD |> countBits) |> ci)))
            |> colorize chart
    | DDR ->
        (chart, fun c (time, nr) ->
            let (ptime,(_, msPerBeat)) = chart.BPM.GetPointAt(time) in (chart, Array.create chart.Keys ((ddr_func (time - ptime) msPerBeat) |> ci)))
            |> colorize chart
    | _ ->
        ((), fun _ _ ->
            ((), Array.zeroCreate(chart.Keys)))
            |> colorize chart