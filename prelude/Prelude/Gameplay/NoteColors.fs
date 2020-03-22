module Prelude.Gameplay.Colors

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



let applyColorizer (scheme : ColorScheme) (colorData : ColorData) (chart : Chart) =
    let ci i = colorData.[i]
    match scheme with
    | Column ->
        for (_, nr) in chart.Notes.Enumerate do
            ()
    | _ -> ()