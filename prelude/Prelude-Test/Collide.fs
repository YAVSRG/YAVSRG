module Collide

open System
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.osu
open Prelude.Charts.ChartConversions
open Prelude.Editor.Filter

(*
let plus ((x1, y1) : Point) ((x2, y2) : Point) =
    (x1 + x2, y1 + y2)

let collide() =
    let receptorOrigin = MultiTimeData<Point>(2)
    let receptorPosition = MultiTimeData<Point>(2)
    let redAngle = TimeData<float>()
    let blueAngle = TimeData<float>()
    let lerp v (x1, y1) (x2, y2) = (x1 * (1.0-v) + x2 * v, y1 * (1.0-v) + y2 * v)
    let lerpf v x1 x2 = x1 * (1.0 - v) + x2 * v
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

    let lin x = x
    let accel x = x * x
    let decel x = 1.0 - (1.0 - x) * (1.0 - x)
    let measure m = 6699.0 + 60000.0 * 4.0 / 143.0 * m
    let arc (start, finish) (dist1, dist2) (angle1, angle2) (spacing1, spacing2) (offset1, offset2) interp =
        let f time =
            let v = interp ((time - start) / (finish - start))
            receptorPosition.GetChannelData(0).InsertAt time (lerp v (angle1, dist1) (angle2, dist2))
            redAngle.InsertAt time ((lerpf v angle1 angle2) + offset1)
            receptorPosition.GetChannelData(1).InsertAt time (lerp v (angle1 + spacing1, dist1) (angle2 + spacing2, dist2))
            blueAngle.InsertAt time ((lerpf v (angle1 + spacing1) (angle2 + spacing2)) + offset2)
        List.iter f [start .. 50.0 .. finish]
    let arcNoAngle (start, finish) (dist1, dist2) (angle1, angle2) (spacing1, spacing2) interp =
        let f time =
            let v = interp ((time - start) / (finish - start))
            receptorPosition.GetChannelData(0).InsertAt time (lerp v (angle1, dist1) (angle2, dist2))
            receptorPosition.GetChannelData(1).InsertAt time (lerp v (angle1 + spacing1, dist1) (angle2 + spacing2, dist2))
        List.iter f [start .. 50.0 .. finish]
    let line (start, finish) p1 p2 interp =
        let f time = 
            let v = interp ((time - start) / (finish - start))
            receptorOrigin.GetChannelData(-1).InsertAt time (lerp v p1 p2)
        List.iter f [start .. 50.0 .. finish]
    //-----------------------------------------------------------
    receptorOrigin.GetChannelData(-1).InsertAt 0.0 (320.0, 340.0)
    receptorOrigin.GetChannelData(0).InsertAt 0.0 (0.0, 0.0)
    receptorOrigin.GetChannelData(1).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(-1).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(0).InsertAt 0.0 (0.0, 0.0)
    receptorPosition.GetChannelData(1).InsertAt 0.0 (0.0, 0.0)
    redAngle.InsertAt 0.0 -0.5
    blueAngle.InsertAt 0.0 -0.5
    
    receptorOrigin.GetChannelData(-1).InsertAt (measure 0.0) (320.0, 240.0)
    arc (measure -2.0, measure 0.0) (100.0, 100.0) (0.8, 1.0) (-0.6, -1.0) (0.5, -0.5) lin
    arc (measure 0.0, measure 1.0) (100.0, 150.0) (1.0, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 1.0, measure 2.0) (150.0, 120.0) (0.9, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 2.0, measure 3.0) (120.0, 150.0) (1.0, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 3.0, measure 4.0) (150.0, 100.0) (1.1, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 4.0, measure 5.0) (100.0, 150.0) (1.0, 0.8) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 5.0, measure 6.0) (150.0, 120.0) (0.8, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 6.0, measure 7.0) (120.0, 150.0) (1.0, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 7.0, measure 8.0) (150.0, 100.0) (1.2, 1.0) (-1.0, -1.0) (0.5, -0.5) lin

    //give me aaaaa
    arc (measure 8.0, measure 8.75) (100.0, 80.0) (1.0, 1.2) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 8.75, measure 10.0) (80.0, 80.0) (1.2, 0.9) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 10.0, measure 10.75) (80.0, 80.0) (0.9, 1.1) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 10.75, measure 12.0) (80.0, 80.0) (1.1, 0.8) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 12.0, measure 12.75) (80.0, 80.0) (0.8, 1.0) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 12.75, measure 14.0) (80.0, 80.0) (1.0, 0.7) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 14.0, measure 14.75) (80.0, 80.0) (0.7, 0.9) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 14.75, measure 15.25) (80.0, 80.0) (0.9, 0.6) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 15.25, measure 15.5) (80.0, 40.0) (0.6, 1.2) (-1.0, -1.0) (0.0, 0.0) decel
    arc (measure 15.5, measure 16.0) (40.0, 100.0) (1.2, 1.0) (-1.0, -1.0) (0.0, 0.0) accel

    //verse 1 - when we collide...
    arc (measure 16.0, measure 18.0) (100.0, 100.0) (1.0, 1.3) (-1.0, -1.0) (0.5, 0.5) accel
    arc (measure 18.0, measure 20.0) (100.0, 100.0) (1.3, 1.6) (-1.0, -1.0) (0.5, 0.5) decel
    arc (measure 20.0, measure 22.0) (100.0, 100.0) (1.6, 1.3) (-1.0, -1.0) (0.5, 0.5) accel
    arc (measure 22.0, measure 24.0) (100.0, 100.0) (1.3, 1.0) (-1.0, -1.0) (0.5, 0.5) decel

    //free fall bridge
    line (measure 24.0, measure 28.0) (320.0, 240.0) (320.0, 360.0) lin
    arc (measure 24.0, measure 28.0) (100.0, 0.0) (1.0, 0.75) (-1.0, -0.5) (0.5, -0.5) lin

    line (measure 31.5, measure 32.0) (320.0, 360.0) (320.0, 240.0) accel
    arc (measure 31.5, measure 32.0) (0.0, 100.0) (1.0, 1.0) (-1.0, -1.0) (0.5, 0.5) accel
    arc (measure 32.0, measure 36.0) (100.0, 100.0) (1.0, -1.0) (-1.0, -1.0) (0.5, 0.5) lin
    line (measure 36.25, measure 36.375) (320.0, 240.0) (400.0, 240.0) decel //five
    line (measure 36.375, measure 36.5) (400.0, 240.0) (320.0, 240.0) accel
    line (measure 36.625, measure 36.75) (320.0, 240.0) (240.0, 240.0) decel
    line (measure 36.75, measure 36.875) (240.0, 240.0) (320.0, 240.0) accel
    line (measure 37.0, measure 37.125) (320.0, 240.0) (320.0, 160.0) decel
    line (measure 37.125, measure 37.25) (320.0, 160.0) (320.0, 240.0) accel
    line (measure 37.25, measure 37.375) (320.0, 240.0) (400.0, 240.0) decel
    line (measure 37.375, measure 37.5) (400.0, 240.0) (320.0, 240.0) accel
    line (measure 37.625, measure 37.75) (320.0, 240.0) (240.0, 240.0) decel
    line (measure 37.75, measure 37.875) (240.0, 240.0) (320.0, 240.0) accel
    arc (measure 36.0, measure 40.0) (100.0, 50.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) lin

    //first drop
    arc (measure 40.0, measure 40.375) (50.0, 150.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 40.375, measure 40.5) (150.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 40.75, measure 41.25) (100.0, 100.0) (1.0, 1.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 41.25, measure 41.5) (100.0, 100.0) (1.5, 0.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 41.5, measure 42.0) (100.0, 50.0) (0.5, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    
    arc (measure 42.0, measure 42.375) (50.0, 150.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 42.375, measure 42.5) (150.0, 0.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 42.75, measure 43.25) (100.0, 100.0) (-1.0, -1.2) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 43.25, measure 43.5) (100.0, 100.0) (-1.2, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 43.5, measure 44.0) (100.0, 50.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) lin

    arc (measure 44.0, measure 44.375) (50.0, 150.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 44.375, measure 44.5) (150.0, 0.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 44.75, measure 45.25) (100.0, 100.0) (-1.0, -1.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 45.25, measure 45.5) (100.0, 100.0) (-1.5, -0.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 45.5, measure 46.0) (100.0, 50.0) (-0.5, 1.0) (-1.0, -1.0) (0.5, -0.5) decel
    //
    arc (measure 47.5, measure 48.375) (50.0, 150.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 48.375, measure 48.5) (150.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 48.75, measure 49.25) (100.0, 100.0) (1.0, 1.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 49.25, measure 49.5) (100.0, 100.0) (1.5, 0.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 49.5, measure 50.0) (100.0, 50.0) (0.5, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    
    arc (measure 50.0, measure 50.375) (50.0, 150.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 50.375, measure 50.5) (150.0, 0.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 50.75, measure 51.25) (100.0, 100.0) (-1.0, -1.2) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 51.25, measure 51.5) (100.0, 100.0) (-1.2, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 51.5, measure 52.0) (100.0, 50.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) lin

    arc (measure 52.0, measure 52.375) (50.0, 150.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 52.375, measure 52.5) (150.0, 0.0) (-1.0, -1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 52.75, measure 53.25) (100.0, 100.0) (-1.0, -1.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 53.25, measure 53.5) (100.0, 100.0) (-1.5, -0.5) (-1.0, -1.0) (0.5, -0.5) lin
    arc (measure 53.5, measure 54.0) (100.0, 50.0) (-0.5, 1.0) (-1.0, -1.0) (0.5, -0.5) decel

    line (measure 54.0, measure 56.0) (320.0, 240.0) (320.0, 360.0) lin
    line (measure 56.0, measure 58.0) (320.0, 360.0) (320.0, 240.0) lin
    //EPIC SYNTH POST-DROP SECTION

    arc (measure 57.5, measure 58.0) (50.0, 150.0) (1.0, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 58.0, measure 58.75) (150.0, 150.0) (1.2, 0.8) (-1.0, -1.0) (0.5, -0.5) accel
    
    arc (measure 58.75, measure 59.0) (150.0, 100.0) (0.8, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 59.0, measure 59.1875) (100.0, 120.0) (1.1, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 59.1875, measure 59.375) (120.0, 80.0) (0.9, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 59.1875, measure 59.375) (120.0, 80.0) (0.9, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 59.375, measure 59.5) (80.0, 120.0) (1.2, 1.0) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 59.5, measure 60.0) (120.0, 100.0) (1.0, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 60.0, measure 64.0) (100.0, 100.0) (1.1, 0.6) (-1.0, -1.0) (0.5, -0.5) accel
    line (measure 60.0, measure 64.0) (320.0, 240.0) (400.0, 320.0) decel
    arc (measure 64.0, measure 68.0) (100.0, 0.0) (0.6, 0.75) (-1.0, -0.5) (0.5, -0.5) accel
    line (measure 64.0, measure 68.0) (400.0, 320.0) (320.0, 240.0) accel
    arc (measure 68.0, measure 72.0) (0.0, 0.0) (0.75, 0.5) (-0.5, -1.0) (0.5, 0.5) lin

    //verse 2 - when we collide
    arc (measure 75.75, measure 76.0) (0.0, 60.0) (0.5, 1.0) (-1.0, -1.0) (0.5, 0.5) lin
    arc (measure 76.0, measure 76.5) (60.0, 80.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) lin
    line (measure 76.0, measure 80.0) (320.0, 240.0) (320.0, 300.0) lin
    arcNoAngle (measure 80.0, measure 86.0) (80.0, 120.0) (1.0, 1.0) (-1.0, -1.0) decel
    arcNoAngle (measure 86.0, measure 86.5) (120.0, 60.0) (1.0, 1.0) (-1.0, -1.0) decel
    arcNoAngle (measure 86.5, measure 87.0) (60.0, 110.0) (1.0, 1.0) (-1.0, -1.0) decel
    arcNoAngle (measure 87.0, measure 87.5) (110.0, 40.0) (1.0, 1.0) (-1.0, -1.0) decel
    arcNoAngle (measure 87.5, measure 88.0) (40.0, 130.0) (1.0, 1.0) (-1.0, -1.0) decel

    let hm m = (measure m, measure (m + 0.5))
    arc (hm 88.0) (130.0, 100.0) (1.0, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 88.5) (100.0, 140.0) (1.2, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 89.0) (140.0, 80.0) (0.9, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 89.5) (80.0, 120.0) (1.1, 0.8) (-1.0, -1.0) (0.5, -0.5) decel

    arc (hm 90.0) (120.0, 100.0) (0.8, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 90.5) (100.0, 140.0) (1.1, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 91.0) (140.0, 80.0) (0.9, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 91.5) (80.0, 120.0) (1.2, 0.8) (-1.0, -1.0) (0.5, -0.5) decel
    
    arc (hm 92.0) (120.0, 100.0) (0.8, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 92.5) (100.0, 140.0) (1.1, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 93.0) (140.0, 80.0) (0.9, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 93.5) (80.0, 120.0) (1.2, 0.8) (-1.0, -1.0) (0.5, -0.5) decel

    arc (hm 94.0) (120.0, 100.0) (0.8, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 94.5) (100.0, 140.0) (1.1, 0.9) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 95.0) (140.0, 80.0) (0.9, 1.2) (-1.0, -1.0) (0.5, -0.5) decel
    arc (hm 95.5) (80.0, 120.0) (1.2, 0.8) (-1.0, -1.0) (0.5, -0.5) decel

    //bridge
    arc (measure 96.0, measure 98.0) (120.0, 110.0) (0.8, 1.1) (-1.0, -1.0) (0.5, -0.5) decel
    arc (measure 98.0, measure 100.0) (110.0, 100.0) (1.1, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    line (measure 98.0, measure 100.0) (320.0, 300.0) (320.0, 240.0) lin
    arc (measure 100.0, measure 103.0) (100.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.0, -0.0) accel

    //drop
    arc (measure 103.875, measure 104.0) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 104.0, measure 104.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 104.09375, measure 104.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 104.1875, measure 104.875) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 104.875, measure 105.0) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 105.0, measure 105.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 105.09375, measure 105.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 105.1875, measure 106.0) (0.0, 40.0) (1.1, -2.0) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 106.0, measure 107.875) (40.0, 80.0) (-2.0, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    
    arc (measure 107.875, measure 108.0) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 108.0, measure 108.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 108.09375, measure 108.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 108.1875, measure 108.875) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 108.875, measure 109.0) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 109.0, measure 109.09375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 109.09375, measure 109.1875) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 109.1875, measure 110.0) (0.0, 40.0) (0.9, 3.0) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 110.0, measure 112.0) (40.0, 80.0) (3.0, 1.0) (-1.0, -1.0) (0.0, -0.0) accel

    arc (measure 112.0, measure 112.25) (80.0, 120.0) (1.0, 1.0) (-1.0, -1.0) (0.5, 0.5) decel
    arc (measure 112.25, measure 112.5) (120.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.5, 0.5) accel
    arc (measure 112.5, measure 113.0) (0.0, 80.0) (1.0, 0.5) (-1.0, -1.0) (0.5, 0.5) decel
    arc (measure 113.0, measure 116.0) (80.0, 80.0) (0.5, 3.5) (-1.0, -1.0) (0.5, 0.5) lin
    arc (measure 116.0, measure 116.25) (80.0, 80.0) (3.5, 2.25) (-1.0, -1.0) (0.5, 0.5) accel
    arc (measure 116.25, measure 116.5) (80.0, 20.0) (2.25, 1.0) (-1.0, -1.0) (0.0, 0.0) decel
    arc (measure 116.5, measure 116.75) (20.0, 80.0) (1.0, 1.0) (-1.0, -1.0) (0.5, -0.5) accel
    arc (measure 117.0, measure 119.75) (80.0, 60.0) (1.0, 0.75) (1.0, 0.5) (0.5, -0.5) accel

    arc (measure 119.875, measure 120.0) (60.0, 0.0) (0.75, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 120.0, measure 120.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 120.09375, measure 120.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 120.1875, measure 121.0) (0.0, 80.0) (1.1, -2.0) (-1.0, -1.0) (0.0, -0.0) decel 
    arc (measure 121.0, measure 121.875) (80.0, 80.0) (-2.0, 1.0) (-1.0, -1.0) (0.0, -0.0) accel

    arc (measure 121.875, measure 122.0) (80.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.0, 0.0) decel
    arc (measure 122.0, measure 122.25) (80.0, 80.0) (1.0, 1.25) (-1.0, -1.0) (0.0, 0.0) accel
    arc (measure 122.25, measure 123.0) (80.0, 80.0) (1.25, 2.0) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 123.0, measure 125.0) (80.0, 80.0) (2.0, 4.0) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 125.0, measure 128.0) (80.0, 80.0) (4.0, 1.0) (-1.0, -1.0) (0.0, 0.0) lin
    arc (measure 128.0, measure 129.75) (80.0, 120.0) (1.0, 0.5) (-1.0, -1.0) (0.0, 0.0) lin

    // end
    arc (measure 129.875, measure 130.0) (120.0, 0.0) (0.5, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 130.0, measure 130.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 130.09375, measure 130.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 130.1875, measure 130.375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 130.375, measure 130.5) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 130.5, measure 130.59375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 130.59375, measure 130.6875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 130.6875, measure 130.875) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 130.875, measure 131.0) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 131.0, measure 131.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 131.09375, measure 131.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 131.1875, measure 131.375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 131.875, measure 132.0) (120.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 132.0, measure 132.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 132.09375, measure 132.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 132.1875, measure 132.375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 132.875, measure 133.0) (80.0, 0.0) (0.5, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 133.0, measure 133.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 133.09375, measure 133.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 133.1875, measure 133.375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin

    arc (measure 133.875, measure 134.0) (80.0, 0.0) (1.5, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 134.0, measure 134.09375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel
    arc (measure 134.09375, measure 134.1875) (80.0, 0.0) (1.0, 1.1) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 134.1875, measure 134.375) (0.0, 80.0) (1.1, 1.0) (-1.0, -1.0) (0.0, -0.0) lin
    
    arc (measure 134.375, measure 134.5) (80.0, 0.0) (1.0, 0.9) (-1.0, -1.0) (0.0, -0.0) decel
    arc (measure 134.5, measure 134.59375) (0.0, 80.0) (0.9, 1.0) (-1.0, -1.0) (0.0, -0.0) accel

    arc (measure 134.6, measure 136.0) (80.0, 0.0) (1.0, 1.0) (-1.0, -1.0) (0.0, -0.0) lin
    //-----------------------------------------------------------

    let receptorPosAt isRed time = 
        let (x,y) = plus (ip time (receptorOrigin.GetChannelData(-1))) (ip time (receptorOrigin.GetChannelData(if isRed then 0 else 1)))
        let (a,d) = plus (ip time (receptorPosition.GetChannelData(-1))) (ip time (receptorPosition.GetChannelData(if isRed then 0 else 1)))
        (x + Math.Cos(Math.PI * a) * d, y + Math.Sin(Math.PI * a) * d)

    let receptorPathing isRed =
        let start = if isRed then 1665.0 else 3343.0
        let finish = 248378.0
        let motion =
            List.map
                (fun t -> t, (receptorPosAt isRed t))
                [start.. 50.0 .. finish]
        (Scale (start, finish, Easing.None, 0.4, 0.4)) :: (Fade (start - 500.0, start, Easing.None, 0.0, 1.0)) :: (Seq.map (fun ((t1, p1),(t2, p2)) -> Move (t1, t2, Easing.None, p1, p2)) (Seq.zip motion (List.tail motion)) |> List.ofSeq) @
            [Fade (finish, (finish + 100.0), Easing.None, 1.0, 0.0)]
    
    let pathing isRed time = 
        let motion =
            List.map
                (fun (t, angle, dist) ->
                    let (x, y) = receptorPosAt isRed t
                    (t, (x + Math.Cos(Math.PI * angle) * dist, y + Math.Sin(Math.PI * angle) * dist)))
                [ for i in (time - 1000.0).. 50.0 .. time -> (i, (if isRed then (fp time redAngle) else (fp time blueAngle)), (time - i) * 0.75) ]

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
    [Sprite (Layer.Overlay, SpriteOrigin.Centre, "receptor.png", (320.0, 240.0),
        (receptorPathing true));
     Sprite (Layer.Overlay, SpriteOrigin.Centre, "receptor.png", (320.0, 240.0),
        (receptorPathing false))] :: (List.map f obj)
    |> List.concat |> saveStoryboardFile @"C:\Users\percy\AppData\Local\osu!\Songs\beatmap-637117532073937730-Fractal - Collide (ft. Danyka Nadeau)\Fractal - Collide (feat. Danyka Nadeau) (Percyqaz).osb"

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
    |> List.map (fun ((t1, m), (t2, _)) -> applyMappingBetween m t1 t2 chart.Notes)
    |> ignore
    Chart(8, chart.Header, chart.Notes, chart.BPM, chart.SV, chart.FileIdentifier)
    |> convert_interlude_osu
    |> saveBeatmapFile @"C:\Users\percy\AppData\Local\osu!\Songs\1122880 WJSN (Cosmic Girls) - Miracle\ *WJSN (Cosmic Girls) - Miracle (Percyqaz) [Dumb Files Ver.].osu"  *)