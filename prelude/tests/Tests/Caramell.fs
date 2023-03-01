module Caramell

open System.IO
open Prelude.Common
open Prelude.Charts.Formats.``osu!``

[<Measure>]
type measure

let source = @"C:\Users\percy\AppData\Local\osu!\Songs\Caramell_-_Caramelldansen_Speedycake_Remix (1)"
let osu_file = Path.Combine(source, "Caramell - Caramelldansen (Speedycake Remix) (Pope Gadget) [THE POPE GADGET VS. PERCYQAZ SHUFFLEGANZA].osu")
let osb_file = Path.Combine("Caramell - Caramelldansen (Speedycake Remix) (Pope Gadget).osb")
let dansen = loadBeatmapFile source

let mutable _msPerBeat = Unchecked.defaultof<_>

let measure =
    match dansen.Timing.Head with
    | BPM (start, msPerBeat, _, _, _) ->
        _msPerBeat <- msPerBeat
        fun (m: float32<measure>) -> m * 4.0f<beat/measure> * msPerBeat + start
    | _ -> failwith "Couldn't find first bpm of osu file"

let intro = 1.0f<measure>
let hook1 = 13.0f<measure>
let verse1_1 = 21.0f<measure>
let verse1_2 = 29.0f<measure>
let chorus1 = 37.0f<measure>
let oo_oo_oowah1 = 45.0f<measure>
let verse2_1 = 53.0f<measure>
let verse2_2 = 61.0f<measure>
let chorus2 = 69.0f<measure>
let hook2 = 77.0f<measure>
let rest = 85.0f<measure>
let buildup = 93.0f<measure>
let oo_oo_oowah2 = 95.0f<measure>
let chorus3_1 = 103.0f<measure>
let chorus3_2 = 111.0f<measure>
let end_of_song = 119.0f<measure>

let main() =
    printfn "%f" (measure end_of_song)