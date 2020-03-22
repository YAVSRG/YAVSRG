module Collide

open System
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.osu
open Prelude.Charts.ChartConversions
open Prelude.Charts.Filter

let plus ((x1, y1) : Point) ((x2, y2) : Point) =
    (x1 + x2, y1 + y2)

let collide =
    let receptorOrigin = MultiTimeData<Point>(2)
    let receptorPosition = MultiTimeData<Point>(2)
    let redAngle = TimeData<float>()
    let blueAngle = TimeData<float>()
    let lerp v (x1, y1) (x2, y2) = (x1 * (1.0-v) + x2 * v, y1 * (1.0-v) + y2 * v)
    let ip time (data : TimeData<Point>) =
        let (time1, p1) = data.GetPointAt(time)
        let (time2, p2) = data.GetNextPointAt(time)
        if (time1 = time2) then p1 else lerp ((time - time1) / (time2 - time1)) p1 p2
    let fp time (data : TimeData<float>) =
        let (time1, p1) = data.GetPointAt(time)
        let (time2, p2) = data.GetNextPointAt(time)
        if (time1 = time2) then p1 else
            let v = ((time - time1) / (time2 - time1))
            p1 * (1.0-v) + p2 * v
        
    receptorOrigin.GetChannelData(-1).InsertAt 0.0 (320.0, 240.0)
    receptorOrigin.GetChannelData(0).InsertAt 0.0 (0.0, 0.0)
    receptorOrigin.GetChannelData(1).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(-1).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(0).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(1).InsertAt 0.0 (0.0, 0.0)
    redAngle.InsertAt 0.0 -0.5
    blueAngle.InsertAt 0.0 -0.5

    let receptorPosAt isRed time = 
        let (x,y) = plus (ip time (receptorOrigin.GetChannelData(-1))) (ip time (receptorOrigin.GetChannelData(if isRed then 0 else 1)))
        let (a,d) = plus (ip time (receptorPosition.GetChannelData(-1))) (ip time (receptorPosition.GetChannelData(if isRed then 0 else 1)))
        (x + Math.Cos(Math.PI * a) * d, y + Math.Sin(Math.PI * a) * d)
    
    let pathing isRed time = 
        let motion =
            List.map
                (fun (t, angle, dist) ->
                    let (x, y) = receptorPosAt isRed t
                    (t, (x + Math.Cos(Math.PI * angle) * dist, y + Math.Sin(Math.PI * angle) * dist)))
                [ for i in (time - 1000.0).. 50.0 .. time -> (i, (if isRed then (fp time redAngle) else (fp time blueAngle)), (time - i) * 0.5) ]

        (Seq.map (fun ((t1, p1),(t2, p2)) -> Move (t1, t2, Easing.None, p1, p2)) (Seq.zip motion (List.tail motion)) |> List.ofSeq) @
            [Fade (time, (time + 100.0), Easing.None, 1.0, 0.0); Scale (time, (time + 100.0), Easing.None, 0.4, 0.7)]

    let holdBody isRed time =
        Sprite (Layer.Overlay, SpriteOrigin.Centre, "hold.png", (320.0, 240.0),
            (Scale ((time - 1000.0), time, Easing.None, 0.4, 0.4)) :: (pathing isRed time))
    let (_, _, _, _, _, obj, _) = 
        @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637117532073937730-Fractal - Collide (ft. Danyka Nadeau)\Fractal - Collide (feat. Danyka Nadeau) (Percyqaz) [Sunrise].osu"
        |> loadBeatmapFile
    let f (obj : HitObject) : StoryboardObject list = 
        match obj with
        | HitCircle ((x,_), time, _, _) ->
            let isRed = x < 320.0
            [Sprite (Layer.Overlay, SpriteOrigin.Centre, (if isRed then "red.png" else "blue.png"), (320.0, 240.0),
                (Scale ((time - 1000.0), time, Easing.None, 0.4, 0.4)) :: (pathing isRed time))]
        | HoldNote ((x, _), time, endTime, _, _) -> 
            let isRed = x < 320.0
            [for t in (time + 50.0) .. 50.0 .. endTime -> holdBody isRed t] @
                [Sprite (Layer.Overlay, SpriteOrigin.Centre, (if isRed then "red.png" else "blue.png"), (320.0, 240.0),
                    (Scale ((time - 1000.0), time, Easing.None, 0.4, 0.4)) :: (pathing isRed time))]
        | _ -> failwith "impossible"
    List.map f obj |> List.concat |> saveStoryboardFile @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637117532073937730-Fractal - Collide (ft. Danyka Nadeau)\Fractal - Collide (feat. Danyka Nadeau) (Percyqaz).osb"

let miracle _ =
    let chart =
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
    |> saveBeatmapFile @"C:\Users\percy\AppData\Local\osu!\Songs\1122880 WJSN (Cosmic Girls) - Miracle\WJSN (Cosmic Girls) - Miracle (Percyqaz) [Dumb Files Ver.].osu"