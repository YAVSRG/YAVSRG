open System
open Prelude.Common;
open Prelude.Charts.Interlude
open Prelude.Charts.osu
open Prelude.Charts.ChartConversions
open Prelude.Charts.Filter
open Prelude.Gameplay.Difficulty
open FParsec

(*
    Prelude features to tackle next:
        Finish .osu storyboard parsing and .osb support
        Finish sm <> interlude conversion
        HP system/life calculation
        Chart modifiers
        Chart coloring systems
        Difficulty rating calculation
        Chart caching tools
*)

[<EntryPoint>]
let main argv =
    (*let chart =
        @"C:\Users\percy\AppData\Local\osu!\Songs\1122880 WJSN (Cosmic Girls) - Miracle\WJSN (Cosmic Girls) - Miracle (Percyqaz) [Uncut Ver.].osu"
        |> loadAndConvertFile |> List.head
    let all = textMapping       "--1234--"
    let luda = textMapping      "-1234---"
    let eunseo = textMapping    "---1234-"
    let chengXiao = textMapping "--12-34-"
    let bona = textMapping      "---1234-"
    let dayoung = textMapping   "----1234"
    let yeonjung = textMapping  "-1234---"
    let seolA = textMapping     "12-34---"
    let dawon = textMapping     "--1234--"
    let soobin = textMapping    "-12-34--"
    let exy = textMapping       "1234----"
    let yeoreum = textMapping   "---1234-"
    let xuanYi = textMapping    "--12-34-"
    let meiQi = textMapping     "--1234--"
    let transitions = [
        (0.0, all);
        (20085.0, luda); (28375.0, eunseo); (32875.0, chengXiao);
        (36375.0, bona); (44375.0, dayoung);
        (52875.0, yeonjung); (62625.0, seolA);
        (69625.0, dawon); (71875.0, soobin); (73625.0, dawon); (76125.0, soobin); (78625.0, yeonjung); 
        (86125.0, exy);
        (104125.0, luda); (108125.0, yeoreum); (112125.0, xuanYi); 
        (120375.0, meiQi); (128125.0, dayoung); 
        (136875.0, yeonjung); (146625.0, seolA); 
        (154125.0, soobin); (162625.0, dawon);
        (170875.0, dayoung); (180625.0, seolA);
        (187625.0, dawon); (189875.0, soobin); (191625.0, dawon); (194125.0, soobin); (196625.0, yeonjung);
        (204085.0, all);
        (220085.0, all)
        ]
    Seq.zip transitions (List.tail transitions) |> List.ofSeq
    |> List.map (fun ((t1, m), (t2, _)) -> applyMappingBetween m t1 t2 chart)
    |> ignore
    Chart(8, chart.Header, chart.Notes, chart.BPM, chart.SV)
    |> convert_interlude_osu
    |> saveBeatmapFile @"C:\Users\percy\AppData\Local\osu!\Songs\1122880 WJSN (Cosmic Girls) - Miracle\WJSN (Cosmic Girls) - Miracle (Percyqaz) [Dumb Files Ver.].osu"*)
    loadStoryboardFile @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637117532073937730-Fractal - Collide (ft. Danyka Nadeau)\Fractal - Collide (feat. Danyka Nadeau) (Percyqaz).osb"
    |> eventsToString
    |> printfn "%A"
    0 