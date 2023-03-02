module Caramell

open System
open System.IO
open Prelude.Common
open Prelude.Charts.Formats.``osu!``

[<Measure>]
type measure

let sprites = @"C:\Users\percy\Desktop\caramelldansen"
let source = @"C:\Users\percy\AppData\Local\osu!\Songs\caramell"
let osu_file = Path.Combine(source, "Caramell - Caramelldansen (Speedycake Remix) (Pope Gadget) [THE POPE GADGET VS. PERCYQAZ SHUFFLEGANZA].osu")
let osb_file = Path.Combine(source, "Caramell - Caramelldansen (Speedycake Remix) (Pope Gadget).osb")
let dansen = loadBeatmapFile osu_file

let mutable _msPerBeat = Unchecked.defaultof<_>

let measure =
    match dansen.Timing.Head with
    | BPM (start, msPerBeat, _, _, _) ->
        _msPerBeat <- msPerBeat
        fun (m: float32<measure>) -> m * 4.0f<beat/measure> * msPerBeat + start
    | _ -> failwith "Couldn't find first bpm of osu file"

let intro = 1.0f<measure> // josh
let hook1 = 11.0f<measure> // josh
let verse1_1 = 19.0f<measure> // josh
let verse1_2 = 27.0f<measure> // blake
let chorus1 = 35.0f<measure> // josh
let oo_oo_oowah1 = 43.0f<measure> // blake
let verse2_1 = 51.0f<measure> // blake
let verse2_2 = 59.0f<measure> // josh
let chorus2 = 67.0f<measure> // blake
let hook2 = 75.0f<measure> // blake
let rest = 83.0f<measure> // josh
let buildup = 91.0f<measure> // josh
let oo_oo_oowah2 = 93.0f<measure> // josh
let chorus3_1 = 101.0f<measure> // blake
let chorus3_2 = 109.0f<measure> // josh
let end_of_song = 117.0f<measure>

let sb = ResizeArray<StoryboardObject>()

// 90 60 130 60  = 340
let frame_times = 
    [90.0f<ms>; 60.0f<ms>; 130.0f<ms>; 60.0f<ms>; 90.0f<ms>; 60.0f<ms>; 130.0f<ms>; 60.0f<ms>]
    |> List.map (fun t -> t / 340.0f<ms/beat> * _msPerBeat)
    |> List.scan (+) 0.0f<ms>

let intro_scene() =
    let blake = 
        [
            Fade (measure 5.0f<measure>, measure 6.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure 6.0f<measure>, measure 7.0f<measure>, Easing.None, 1.0, 0.0)
            Move_X (measure 5.0f<measure>, measure 7.0f<measure>, Easing.None, 520, 120)
            Rotate (measure 5.0f<measure>, measure 7.0f<measure>, Easing.None, Math.PI * 0.125, Math.PI * -0.125)

            Fade (measure 9.0f<measure>, measure 10.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure 10.0f<measure>, measure 11.0f<measure>, Easing.None, 1.0, 0.0)
            Move_X (measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 80, 480)
            Rotate (measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, Math.PI * -0.125, Math.PI * 0.125)
        ]
    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "blake-1.png", (480, 500), blake) |> sb.Add

    let josh = 
        [
            Fade (measure 7.0f<measure>, measure 8.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure 8.0f<measure>, measure 9.0f<measure>, Easing.None, 1.0, 0.0)
            Move_X (measure 7.0f<measure>, measure 9.0f<measure>, Easing.None, 120, 520)
            Rotate (measure 7.0f<measure>, measure 9.0f<measure>, Easing.None, Math.PI * -0.125, Math.PI * 0.125)
            
            Fade (measure 9.0f<measure>, measure 10.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure 10.0f<measure>, measure 11.0f<measure>, Easing.None, 1.0, 0.0)
            Move_X (measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 480, 80)
            Rotate (measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, Math.PI * 0.125, Math.PI * -0.125)
        ]
    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "josh-5.png", (480, 500), josh) |> sb.Add

let disco_ball() =
    let events =
        seq {
            yield Rotate(0.0f<ms>, measure end_of_song, Easing.None, 0.0, Math.PI * 1.3 * float end_of_song)
            let mutable last = 0.0f<ms>
            let mutable last_was_kiai = false
            for p in dansen.Timing do
                match p with
                | SV (t, _, _, effect) -> 
                    if effect = TimingEffect.Kiai then 
                        yield Fade(last, t, Easing.None, 0.0, 0.0)
                    else yield Fade(last, t, Easing.None, 1.0, 1.0)
                    last <- t
                    last_was_kiai <- effect = TimingEffect.Kiai
                | _ -> ()
        } |> List.ofSeq
    Sprite(Layer.Foreground, SpriteOrigin.Centre, "rays.png", (320, 80), events)
    |> sb.Add

let main_scene() =
    let bg =
        [
            Fade (measure 0.0f<measure>, measure 1.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure end_of_song, measure (end_of_song + 1.0f<measure>), Easing.None, 1.0, 0.0)
        ]
    Sprite(Layer.Foreground, SpriteOrigin.Centre, "background.png", (320, 240), bg)
    |> sb.Add

    let dance_floor =
        [
            Scale (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.8, 0.8)
            Move_Y (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 520, 300)
            Fade (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure end_of_song, measure (end_of_song + 1.0f<measure>), Easing.None, 1.0, 0.0)
        ]
    let dance_floor_bounces =
        seq {
            let mutable t = hook1
            while t < rest do
                t <- t + 0.25f<measure>
                yield VectorScale(measure t, measure (t + 0.125f<measure>), Easing.Decelerate, (0.8, 0.8), (0.8, 1.0))
                yield VectorScale(measure (t + 0.125f<measure>), measure (t + 0.25f<measure>), Easing.Accelerate, (0.8, 1.0), (0.8, 0.8))
            t <- oo_oo_oowah2
            while t < end_of_song do
                t <- t + 0.25f<measure>
                yield VectorScale(measure t, measure (t + 0.125f<measure>), Easing.Decelerate, (0.8, 0.8), (0.8, 1.0))
                yield VectorScale(measure (t + 0.125f<measure>), measure (t + 0.25f<measure>), Easing.Accelerate, (0.8, 1.0), (0.8, 0.8))
        } |> List.ofSeq

    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "dance_floor.png", (320, 520), dance_floor @ dance_floor_bounces)
    |> sb.Add

    disco_ball()

    let disco_ball =
        [
            Scale (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.5, 0.5)
            Move_Y (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, -100, 0)
            Fade (measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.0, 1.0)
            Fade (measure end_of_song, measure (end_of_song + 1.0f<measure>), Easing.None, 1.0, 0.0)
        ]
    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "disco_ball.png", (320, 0), disco_ball)
    |> sb.Add

let dance_ext (who: string) (time: float32<measure>) (repeat: int) (scale1: float, scale2: float) (x_pos1: int, x_pos2: int) (y_pos: int) =
    for i = 1 to 8 do
        let events =
            seq {
                let start_time = measure time
                let end_time = measure (time + 0.5f<measure> * float32 repeat)
                yield Fade(start_time, start_time + frame_times.[i - 1], Easing.None, 0.0, 0.0)
                yield Scale(start_time, end_time, Easing.None, scale1, scale2)
                yield Move_X(start_time, end_time, Easing.None, x_pos1, x_pos2)
                for x in 0 .. repeat - 1 do
                    let b = measure (time + 0.5f<measure> * float32 x)
                    let t1 = b + frame_times.[i - 1]
                    let t2 = b + frame_times.[i]
                    yield Fade(t1, t2, Easing.None, 1.0, 1.0)
                    yield Fade(t2, t2 + _msPerBeat * 1.0f<beat>, Easing.None, 0.0, 0.0)
            } |> List.ofSeq
        Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, who + "-" + i.ToString() + ".png", (x_pos1, y_pos), events)
        |> sb.Add

let dance (who: string) (time: float32<measure>) (repeat: int) (scale1: float, scale2: float) (x_pos1: int, x_pos2: int) =
    dance_ext who time repeat (scale1, scale2) (x_pos1, x_pos2) 480

let camera_1 (time: float32<measure>) (main: string) =
    
    let support = if main = "josh" then "blake" else "josh"
    
    let spacing = 160
    let movement = 640
    for i = -1 to (640 + movement) / spacing / 2 do
        let j = 2 * i
        dance support time 4 (0.6, 0.6) (-movement + j * spacing, j * spacing)
    for i = -1 to (640 + movement) / spacing / 2 do
        let j = 2 * i + 1
        dance main time 4 (0.8, 0.8) (-movement + j * spacing, j * spacing)

let camera_2 (time: float32<measure>) (main: string) =
    
    let support = if main = "josh" then "blake" else "josh"
    
    dance support time 4 (0.5, 0.5) (640, 0)
    dance_ext main time 4 (1.2, 1.2) (0, 640) 680

let camera_3 (time: float32<measure>) (main: string) =
    
    let support = if main = "josh" then "blake" else "josh"
    
    dance support time 4 (0.4, 0.5) (300, 500)
    dance support time 4 (0.5, 0.4) (100, 300)
    dance main time 4 (0.85, 0.95) (320, 320)
    dance support time 4 (0.6, 0.5) (340, 140)
    dance support time 4 (0.5, 0.6) (540, 340)
    
let camera_4 (time: float32<measure>) (main: string) =
    dance_ext main time 4 (1.5, 1.7) (320, 320) 900

let main() =
    // copy files
    for file in Directory.GetFiles sprites do
        if file.EndsWith(".png") then
            try File.Copy(file, Path.Combine(source, Path.GetFileName file)) with _ -> ()

    // set up backdrop
    main_scene()
    intro_scene()
    
    // hook
    camera_1 hook1 "josh"
    camera_1 (hook1 + 2.0f<measure>) "josh"
    camera_1 (hook1 + 4.0f<measure>) "josh"
    camera_1 (hook1 + 6.0f<measure>) "josh"

    // verse 1
    camera_4 verse1_1 "josh"
    camera_3 (verse1_1 + 2.0f<measure>) "josh"
    camera_2 (verse1_1 + 4.0f<measure>) "josh"
    camera_1 (verse1_1 + 6.0f<measure>) "josh"

    camera_3 verse1_2 "blake"
    camera_1 (verse1_2 + 2.0f<measure>) "blake"
    camera_4 (verse1_2 + 4.0f<measure>) "blake"
    camera_2 (verse1_2 + 6.0f<measure>) "blake"

    // chorus 1
    camera_2 chorus1 "josh"
    camera_1 (chorus1 + 2.0f<measure>) "josh"
    camera_3 (chorus1 + 4.0f<measure>) "josh"
    camera_4 (chorus1 + 6.0f<measure>) "josh"

    // oo oo oowah
    camera_3 oo_oo_oowah1 "blake"
    camera_1 (oo_oo_oowah1 + 2.0f<measure>) "blake"
    camera_4 (oo_oo_oowah1 + 4.0f<measure>) "blake"
    camera_2 (oo_oo_oowah1 + 6.0f<measure>) "blake"
    
    // verse 2
    camera_1 verse2_1 "blake"
    camera_3 (verse2_1 + 2.0f<measure>) "blake"
    camera_4 (verse2_1 + 4.0f<measure>) "blake"
    camera_2 (verse2_1 + 6.0f<measure>) "blake"
    
    camera_3 verse2_2 "josh"
    camera_2 (verse2_2 + 2.0f<measure>) "josh"
    camera_1 (verse2_2 + 4.0f<measure>) "josh"
    camera_4 (verse2_2 + 6.0f<measure>) "josh"
    
    // chorus 2
    camera_3 chorus2 "blake"
    camera_4 (chorus2 + 2.0f<measure>) "blake"
    camera_1 (chorus2 + 4.0f<measure>) "blake"
    camera_2 (chorus2 + 6.0f<measure>) "blake"
    
    // hook 2
    camera_1 hook2 "blake"
    camera_1 (hook2 + 2.0f<measure>) "blake"
    camera_1 (hook2 + 4.0f<measure>) "blake"
    camera_1 (hook2 + 6.0f<measure>) "blake"
    
    // rest TBC
    
    /// alternate faces really fast

    // oo oo oowah 2
    camera_1 oo_oo_oowah2 "josh"
    camera_1 (oo_oo_oowah2 + 2.0f<measure>) "josh"
    camera_1 (oo_oo_oowah2 + 4.0f<measure>) "josh"
    camera_1 (oo_oo_oowah2 + 6.0f<measure>) "josh"

    // final chorus
    /// custom camera angles incorporating us both

    // save
    saveStoryboardFile osb_file (List.ofSeq sb)

(* todo list

bouncing stars
disco ball animation
dance floor animation
stuff in break section
stuff in final chorus

 *)