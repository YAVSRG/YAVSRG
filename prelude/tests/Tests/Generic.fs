namespace Prelude.Test

open System
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Data.Charts

module Generic =
    
    let dayDreamerSM = @"C:\Users\percy\Desktop\Source\YAVSRG\Interlude\bin\Debug\netcoreapp3.1\Songs\Compulsive Chordjack Collection 2\Day Dreamer (XingRen)\Dance_Single 23 [0].yav"
    let dayDreamerOsu = @"C:\Users\percy\Desktop\Source\YAVSRG\Interlude\bin\Debug\netcoreapp3.1\Songs\osu!\Day Dreamer (XingRen)\Day Dreamer [(Xingren) 1.0x - 22.94 MSD].yav"

    let main() =

        let a = Chart.fromFile dayDreamerSM
        let b = Chart.fromFile dayDreamerOsu

        for ((time1, left), (time2, right)) in Seq.zip a.Value.Notes.Data b.Value.Notes.Data do
            printfn "%f: %s\t\t| %f: %s" time1 (NoteRow.prettyPrint left) time2 (NoteRow.prettyPrint right)