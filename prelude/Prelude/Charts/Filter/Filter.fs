module Prelude.Charts.Filter

open Prelude.Charts.Interlude

let mirror time1 time2 (chart : Chart) : Chart = 
    let mirrorBits (bm : Bitmap) : Bitmap =
        makeBitmap (Seq.map (fun x -> chart.Keys - 1 - x) (getBits bm))
    
    for (_, nr) in chart.Notes.EnumerateBetween time1 time2 do
        for i = 0 to 4 do nr.[i] <- mirrorBits nr.[i]
    chart