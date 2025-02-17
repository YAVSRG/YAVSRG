module Caramell

open System
open System.IO
open Prelude
open Prelude.Formats.Osu
[<Measure>]
type measure

let sprites = @"C:\Users\percy\Desktop\caramelldansen"
let source = @"C:\Users\percy\AppData\Local\osu!\Songs\caramell"

let osu_file =
    Path.Combine(
        source,
        "Caramell - Caramelldansen (Speedycake Remix) (Pope Gadget) [THE POPE GADGET VS. PERCYQAZ SHUFFLEGANZA 1.05x].osu"
    )

let dansen = Beatmap.FromFile osu_file |> Result.toOption |> Option.get

let mutable _msPerBeat = Unchecked.defaultof<_>

let measure =
    match dansen.Timing.Head with
    | Uninherited x ->
        _msPerBeat <- Time.of_number x.MsPerBeat / 1.0f<beat>
        fun (m: float32<measure>) -> m * 4.0f<beat / measure> * _msPerBeat + Time.of_number x.Time
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
    [
        90.0f<ms>
        60.0f<ms>
        130.0f<ms>
        60.0f<ms>
        90.0f<ms>
        60.0f<ms>
        130.0f<ms>
        60.0f<ms>
    ]
    |> List.map (fun t -> t / 340.0f<ms / beat> * _msPerBeat)
    |> List.scan (+) 0.0f<ms>

//let break_intro (time: float32<measure>) =
//    let measure1 (x: int) =
//        measure (float32 x * 1.0f<measure> + time)

//    let blake =
//        [
//            Fade(measure1 0, measure1 1, Easing.None, 0.0, 1.0)
//            Fade(measure1 1, measure1 2, Easing.None, 1.0, 0.0)
//            Move_X(measure1 0, measure1 2, Easing.None, 80, 480)
//            Rotate(measure1 0, measure1 2, Easing.None, Math.PI * -0.125, Math.PI * 0.125)

//            Fade(measure1 4, measure1 5, Easing.None, 0.0, 1.0)
//            Fade(measure1 5, measure1 6, Easing.None, 1.0, 0.0)
//            Move_X(measure1 4, measure1 6, Easing.None, 480, 80)
//            Rotate(measure1 4, measure1 6, Easing.None, Math.PI * 0.125, Math.PI * -0.125)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "blake-1.png", (480, 500), blake)
//    |> sb.Add

//    let josh =
//        [
//            Fade(measure1 2, measure1 3, Easing.None, 0.0, 1.0)
//            Fade(measure1 3, measure1 4, Easing.None, 1.0, 0.0)
//            Move_X(measure1 2, measure1 4, Easing.None, 480, 80)
//            Rotate(measure1 2, measure1 4, Easing.None, Math.PI * 0.125, Math.PI * -0.125)

//            Fade(measure1 4, measure1 5, Easing.None, 0.0, 1.0)
//            Fade(measure1 5, measure1 6, Easing.None, 1.0, 0.0)
//            Move_X(measure1 4, measure1 6, Easing.None, 80, 480)
//            Rotate(measure1 4, measure1 6, Easing.None, Math.PI * -0.125, Math.PI * 0.125)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "josh-5.png", (480, 500), josh)
//    |> sb.Add

//let intro_scene () =
//    let blake =
//        [
//            Fade(measure 5.0f<measure>, measure 6.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 6.0f<measure>, measure 7.0f<measure>, Easing.None, 1.0, 0.0)
//            Move_X(measure 5.0f<measure>, measure 7.0f<measure>, Easing.None, 520, 120)
//            Rotate(measure 5.0f<measure>, measure 7.0f<measure>, Easing.None, Math.PI * 0.125, Math.PI * -0.125)

//            Fade(measure 9.0f<measure>, measure 10.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 10.0f<measure>, measure 11.0f<measure>, Easing.None, 1.0, 0.0)
//            Move_X(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 80, 480)
//            Rotate(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, Math.PI * -0.125, Math.PI * 0.125)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "blake-1.png", (480, 500), blake)
//    |> sb.Add

//    let josh =
//        [
//            Fade(measure 7.0f<measure>, measure 8.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 8.0f<measure>, measure 9.0f<measure>, Easing.None, 1.0, 0.0)
//            Move_X(measure 7.0f<measure>, measure 9.0f<measure>, Easing.None, 120, 520)
//            Rotate(measure 7.0f<measure>, measure 9.0f<measure>, Easing.None, Math.PI * -0.125, Math.PI * 0.125)

//            Fade(measure 9.0f<measure>, measure 10.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 10.0f<measure>, measure 11.0f<measure>, Easing.None, 1.0, 0.0)
//            Move_X(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 480, 80)
//            Rotate(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, Math.PI * 0.125, Math.PI * -0.125)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, "josh-5.png", (480, 500), josh)
//    |> sb.Add

//let disco_ball () =
//    let events =
//        seq {
//            yield Rotate(0.0f<ms>, measure end_of_song, Easing.None, 0.0, Math.PI * 1.3 * float end_of_song)
//            let mutable last = 0.0f<ms>
//            let mutable last_was_kiai = false

//            for p in dansen.Timing do
//                match p with
//                | SV(t, _, _, effect) ->
//                    if effect = TimingEffect.Kiai then
//                        yield Fade(last, t, Easing.None, 0.0, 0.0)
//                    else
//                        yield Fade(last, t, Easing.None, 1.0, 1.0)

//                    last <- t
//                    last_was_kiai <- effect = TimingEffect.Kiai
//                | _ -> ()
//        }
//        |> List.ofSeq

//    Sprite(Layer.Foreground, SpriteOrigin.Centre, "rays.png", (320, 80), events)
//    |> sb.Add

//let main_scene () =
//    let bg =
//        [
//            Fade(measure 0.0f<measure>, measure 1.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 11.0f<measure>, measure (12.0f<measure>), Easing.None, 1.0, 0.0)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.Centre, "background-dark.png", (320, 240), bg)
//    |> sb.Add

//    let bg2 =
//        [
//            Fade(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure end_of_song, measure (end_of_song + 1.0f<measure>), Easing.None, 1.0, 0.0)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.Centre, "background.png", (320, 240), bg2)
//    |> sb.Add

//    let dance_floor =
//        [
//            Scale(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.8, 0.8)
//            Move_Y(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 520, 300)
//            Fade(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure 5.0f<measure>, measure 11.0f<measure>, Easing.None, 1.0, 1.0)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "dancefloor-0.png", (320, 520), dance_floor)
//    |> sb.Add

//    let dance_floor2 =
//        [
//            Scale(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 0.8, 0.8)
//            Fade(measure 9.0f<measure>, measure 11.0f<measure>, Easing.None, 0.0, 1.0)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "dancefloor-1-1.png", (320, 300), dance_floor2)
//    |> sb.Add

//    let dance_floor_bounces i j =
//        seq {
//            let mutable t = hook1

//            while t < rest do
//                yield VectorScale(measure t, measure (t + 0.125f<measure>), Easing.Decelerate, (0.8, 0.8), (0.8, 1.0))

//                yield
//                    VectorScale(
//                        measure (t + 0.125f<measure>),
//                        measure (t + 0.25f<measure>),
//                        Easing.Accelerate,
//                        (0.8, 1.0),
//                        (0.8, 0.8)
//                    )

//                let which_i = MathF.Floor((t - hook1) % 32.0f<measure> / 8.0f<measure>) |> int
//                let which_j = MathF.Floor((t - hook1) % 1.0f<measure> / 0.25f<measure>) |> int

//                let f = if i - 1 = which_i && j - 1 = which_j then 1.0 else 0.0
//                yield Fade(measure t, measure (t + 0.25f<measure>), Easing.None, f, f)
//                t <- t + 0.25f<measure>

//            t <- oo_oo_oowah2

//            while t < end_of_song do
//                yield VectorScale(measure t, measure (t + 0.125f<measure>), Easing.Decelerate, (0.8, 0.8), (0.8, 1.0))

//                yield
//                    VectorScale(
//                        measure (t + 0.125f<measure>),
//                        measure (t + 0.25f<measure>),
//                        Easing.Accelerate,
//                        (0.8, 1.0),
//                        (0.8, 0.8)
//                    )

//                let which_i =
//                    MathF.Floor((t - oo_oo_oowah2) % 32.0f<measure> / 8.0f<measure>) |> int

//                let which_j =
//                    MathF.Floor((t - oo_oo_oowah2) % 1.0f<measure> / 0.25f<measure>) |> int

//                let f = if i - 1 = which_i && j - 1 = which_j then 1.0 else 0.0
//                yield Fade(measure t, measure (t + 0.25f<measure>), Easing.None, f, f)
//                t <- t + 0.25f<measure>
//        }
//        |> List.ofSeq

//    for i = 1 to 4 do
//        for j = 1 to 4 do
//            Sprite(
//                Layer.Foreground,
//                SpriteOrigin.TopCentre,
//                sprintf "dancefloor-%i-%i.png" i j,
//                (320, 300),
//                dance_floor_bounces i j
//            )
//            |> sb.Add

//    disco_ball ()

//    let disco_ball =
//        [
//            Scale(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.5, 0.5)
//            Move_Y(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, -100, 0)
//            Fade(measure 1.0f<measure>, measure 5.0f<measure>, Easing.None, 0.0, 1.0)
//            Fade(measure end_of_song, measure (end_of_song + 1.0f<measure>), Easing.None, 1.0, 0.0)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "disco_ball-1.png", (320, 0), disco_ball)
//    |> sb.Add

//    let db_frames i =
//        seq {
//            yield
//                Scale(
//                    measure (hook1 + float32 i * 0.1f<measure>),
//                    measure (hook1 + float32 i * 0.1f<measure>),
//                    Easing.None,
//                    0.5,
//                    0.5
//                )

//            let mutable t = hook1

//            while t < rest do
//                let f =
//                    if (MathF.Round(t / 0.1f<measure>) |> int) % 4 + 1 = i then
//                        1.0
//                    else
//                        0.0

//                yield Fade(measure t, measure (t + 0.1f<measure>), Easing.None, f, f)
//                t <- t + 0.1f<measure>

//            t <- oo_oo_oowah2

//            while t < end_of_song do
//                let f =
//                    if (MathF.Round(t / 0.1f<measure>) |> int) % 4 + 1 = i then
//                        1.0
//                    else
//                        0.0

//                yield Fade(measure t, measure (t + 0.1f<measure>), Easing.None, f, f)
//                t <- t + 0.1f<measure>
//        }
//        |> List.ofSeq

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "disco_ball-2.png", (320, 0), db_frames 2)
//    |> sb.Add

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "disco_ball-3.png", (320, 0), db_frames 3)
//    |> sb.Add

//    Sprite(Layer.Foreground, SpriteOrigin.TopCentre, "disco_ball-4.png", (320, 0), db_frames 4)
//    |> sb.Add

//let dance_ext
//    (who: string)
//    (time: float32<measure>)
//    (repeat: int)
//    (scale1: float, scale2: float)
//    (x_pos1: int, x_pos2: int)
//    (y_pos: int)
//    =
//    for i = 1 to 8 do
//        let events =
//            seq {
//                let start_time = measure time
//                let end_time = measure (time + 0.5f<measure> * float32 repeat)
//                yield Fade(start_time, start_time + frame_times.[i - 1], Easing.None, 0.0, 0.0)
//                yield Scale(start_time, end_time, Easing.None, scale1, scale2)
//                yield Move_X(start_time, end_time, Easing.None, x_pos1, x_pos2)

//                for x in 0 .. repeat - 1 do
//                    let b = measure (time + 0.5f<measure> * float32 x)
//                    let t1 = b + frame_times.[i - 1]
//                    let t2 = b + frame_times.[i]
//                    yield Fade(t1, t2, Easing.None, 1.0, 1.0)
//                    yield Fade(t2, t2 + _msPerBeat * 1.0f<beat>, Easing.None, 0.0, 0.0)
//            }
//            |> List.ofSeq

//        Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, who + "-" + i.ToString() + ".png", (x_pos1, y_pos), events)
//        |> sb.Add

//let dance
//    (who: string)
//    (time: float32<measure>)
//    (repeat: int)
//    (scale1: float, scale2: float)
//    (x_pos1: int, x_pos2: int)
//    =
//    dance_ext who time repeat (scale1, scale2) (x_pos1, x_pos2) 480

//let star (x: int, y: int) (scale: float) (time: float32<measure>) (repeat: int) =
//    let events =
//        seq {
//            let start_time = measure time
//            let end_time = measure (time + 0.5f<measure> * float32 repeat)
//            yield Scale(start_time, end_time, Easing.None, scale, scale)

//            for x in 0 .. repeat - 1 do
//                let t1 = measure (time + 0.5f<measure> * float32 x)
//                let t2 = measure (time + + 0.125f<measure> + 0.5f<measure> * float32 x)
//                let t3 = measure (time + + 0.25f<measure> + 0.5f<measure> * float32 x)
//                let t4 = measure (time + + 0.375f<measure> + 0.5f<measure> * float32 x)
//                let t5 = measure (time + + 0.5f<measure> + 0.5f<measure> * float32 x)

//                yield Move_Y(t1, t2, Easing.Decelerate, float y + 50.0, float y)
//                yield Rotate(t1, t2, Easing.Decelerate, 0.3, 0.0)
//                yield Move_Y(t2, t3, Easing.Accelerate, float y, float y + 50.0)
//                yield Rotate(t2, t3, Easing.Accelerate, 0.0, -0.3)

//                yield Move_Y(t3, t4, Easing.Decelerate, float y + 50.0, float y)
//                yield Rotate(t3, t4, Easing.Decelerate, -0.3, 0.0)
//                yield Move_Y(t4, t5, Easing.Accelerate, float y, float y + 50.0)
//                yield Rotate(t4, t5, Easing.Accelerate, 0.0, 0.3)
//        }
//        |> List.ofSeq

//    Sprite(Layer.Foreground, SpriteOrigin.Centre, "star.png", (x, y), events)
//    |> sb.Add

//let stars (time: float32<measure>) =
//    star (320, 200) 0.7 time 16
//    star (145, 300) 0.75 time 16
//    star (495, 300) 0.75 time 16
//    star (-30, 240) 0.7 time 16
//    star (670, 240) 0.7 time 16

//let camera_1 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    let spacing = 160
//    let movement = 640

//    for i = -1 to (640 + movement) / spacing / 2 do
//        let j = 2 * i
//        dance support time 4 (0.6, 0.6) (-movement + j * spacing, j * spacing)

//    for i = -1 to (640 + movement) / spacing / 2 do
//        let j = 2 * i + 1
//        dance main time 4 (0.8, 0.8) (-movement + j * spacing, j * spacing)

//let camera_2 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    dance support time 4 (0.5, 0.5) (640, 0)
//    dance_ext main time 4 (1.2, 1.2) (0, 640) 680

//let camera_3 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    dance support time 4 (0.4, 0.5) (300, 500)
//    dance support time 4 (0.5, 0.4) (100, 300)
//    dance main time 4 (0.85, 0.95) (320, 320)
//    dance support time 4 (0.6, 0.5) (340, 140)
//    dance support time 4 (0.5, 0.6) (540, 340)

//let camera_4 (time: float32<measure>) (main: string) =

//    dance_ext main time 4 (1.5, 1.7) (320, 320) 900

//let special_camera_1 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    dance_ext main time 2 (1.0, 1.0) (100, 100) 600
//    dance_ext support time 2 (1.0, 1.0) (540, 540) 600

//let special_camera_2 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    let spacing = 320
//    let movement = 320

//    for i = -1 to (640 + movement) / spacing do
//        dance (if i % 2 = 0 then support else main) time 2 (0.8, 0.8) (-movement + i * spacing, i * spacing)

//let special_camera_3 (time: float32<measure>) (main: string) =

//    let support = if main = "josh" then "blake" else "josh"

//    dance support time 1 (0.7, 0.7) (120, 120)
//    dance support time 1 (0.7, 0.7) (520, 520)
//    dance main time 1 (0.9, 0.9) (320, 320)

//    dance main (time + 0.5f<measure>) 1 (0.5, 0.7) (320, 520)
//    dance support (time + 0.5f<measure>) 1 (0.7, 0.5) (120, 320)
//    dance main (time + 0.5f<measure>) 1 (0.9, 0.7) (320, 120)
//    dance support (time + 0.5f<measure>) 1 (0.7, 0.9) (520, 320)

//let face (who: string) (i: int) (fade: bool) (time: float32<measure>) (duration: float32<measure>) =

//    let events =
//        [
//            Fade(measure time, measure (time + duration), Easing.Accelerate, 1.0, (if fade then 0.0 else 1.0))
//            Scale(measure time, measure (time + duration), Easing.None, 1.5, 1.5)
//        ]

//    Sprite(Layer.Foreground, SpriteOrigin.BottomCentre, who + "-" + i.ToString() + ".png", (320, 820), events)
//    |> sb.Add

//let main () =
//    // copy files
//    for file in Directory.GetFiles sprites do
//        if file.EndsWith(".png") then
//            try
//                File.Copy(file, Path.Combine(source, Path.GetFileName file))
//            with _ ->
//                ()

//    // set up backdrop
//    main_scene ()
//    intro_scene ()

//    // hook
//    stars hook1
//    camera_1 hook1 "josh"
//    camera_1 (hook1 + 2.0f<measure>) "josh"
//    camera_1 (hook1 + 4.0f<measure>) "josh"
//    camera_1 (hook1 + 6.0f<measure>) "josh"

//    // verse 1
//    camera_4 verse1_1 "josh"
//    camera_3 (verse1_1 + 2.0f<measure>) "josh"
//    camera_2 (verse1_1 + 4.0f<measure>) "josh"
//    camera_1 (verse1_1 + 6.0f<measure>) "josh"

//    camera_3 verse1_2 "blake"
//    camera_1 (verse1_2 + 2.0f<measure>) "blake"
//    camera_4 (verse1_2 + 4.0f<measure>) "blake"
//    camera_2 (verse1_2 + 6.0f<measure>) "blake"

//    // chorus 1
//    stars chorus1
//    camera_2 chorus1 "josh"
//    camera_1 (chorus1 + 2.0f<measure>) "josh"
//    camera_3 (chorus1 + 4.0f<measure>) "josh"
//    camera_4 (chorus1 + 6.0f<measure>) "josh"

//    // oo oo oowah
//    stars oo_oo_oowah1
//    camera_3 oo_oo_oowah1 "blake"
//    camera_1 (oo_oo_oowah1 + 2.0f<measure>) "blake"
//    camera_4 (oo_oo_oowah1 + 4.0f<measure>) "blake"
//    camera_2 (oo_oo_oowah1 + 6.0f<measure>) "blake"

//    // verse 2
//    camera_1 verse2_1 "blake"
//    camera_3 (verse2_1 + 2.0f<measure>) "blake"
//    camera_4 (verse2_1 + 4.0f<measure>) "blake"
//    camera_2 (verse2_1 + 6.0f<measure>) "blake"

//    camera_3 verse2_2 "josh"
//    camera_2 (verse2_2 + 2.0f<measure>) "josh"
//    camera_1 (verse2_2 + 4.0f<measure>) "josh"
//    camera_4 (verse2_2 + 6.0f<measure>) "josh"

//    // chorus 2
//    stars chorus2
//    camera_3 chorus2 "blake"
//    camera_4 (chorus2 + 2.0f<measure>) "blake"
//    camera_1 (chorus2 + 4.0f<measure>) "blake"
//    camera_2 (chorus2 + 6.0f<measure>) "blake"

//    // hook 2
//    stars hook2
//    camera_1 hook2 "blake"
//    camera_1 (hook2 + 2.0f<measure>) "blake"
//    camera_1 (hook2 + 4.0f<measure>) "blake"
//    camera_1 (hook2 + 6.0f<measure>) "blake"

//    // rest
//    break_intro rest

//    face "josh" 5 true (rest + 8.0f<measure>) 1.0f<measure>
//    face "blake" 1 true (rest + 9.0f<measure>) 0.25f<measure>
//    face "josh" 5 true (rest + 9.25f<measure>) 0.25f<measure>

//    for i = 1 to 8 do
//        face
//            (if i % 2 = 0 then "josh" else "blake")
//            i
//            false
//            (rest + 9.4375f<measure> + 0.0625f<measure> * float32 i)
//            0.0625f<measure>

//    // oo oo oowah 2
//    stars oo_oo_oowah2
//    camera_1 oo_oo_oowah2 "josh"
//    camera_1 (oo_oo_oowah2 + 2.0f<measure>) "josh"
//    camera_1 (oo_oo_oowah2 + 4.0f<measure>) "josh"
//    camera_1 (oo_oo_oowah2 + 6.0f<measure>) "josh"

//    // final chorus
//    stars chorus3_1
//    special_camera_3 (chorus3_1 + 0.0f<measure>) "josh"
//    special_camera_3 (chorus3_1 + 1.0f<measure>) "blake"
//    special_camera_2 (chorus3_1 + 2.0f<measure>) "blake"
//    special_camera_2 (chorus3_1 + 3.0f<measure>) "josh"
//    special_camera_1 (chorus3_1 + 4.0f<measure>) "josh"
//    special_camera_1 (chorus3_1 + 5.0f<measure>) "josh"
//    special_camera_2 (chorus3_1 + 6.0f<measure>) "josh"
//    special_camera_2 (chorus3_1 + 7.0f<measure>) "blake"

//    stars chorus3_2
//    special_camera_3 (chorus3_2 + 0.0f<measure>) "josh"
//    special_camera_2 (chorus3_2 + 1.0f<measure>) "blake"
//    special_camera_1 (chorus3_2 + 2.0f<measure>) "blake"
//    special_camera_3 (chorus3_2 + 3.0f<measure>) "blake"
//    special_camera_2 (chorus3_2 + 4.0f<measure>) "josh"
//    special_camera_1 (chorus3_2 + 5.0f<measure>) "josh"
//    special_camera_2 (chorus3_2 + 6.0f<measure>) "blake"
//    special_camera_2 (chorus3_2 + 7.0f<measure>) "josh"

//    // save
//    let result =
//        { dansen with
//            Events = List.take 2 dansen.Events @ List.ofSeq sb
//        }

//    beatmap_to_file osu_file result