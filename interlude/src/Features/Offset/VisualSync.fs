namespace Interlude.Features.Offset

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Interlude.Options
open Interlude
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features

type VisualOffsetPart2() =
    inherit Page()

    override this.Init(parent) =
        this.Content (
            column()
            //|+ PageButton("offset.visualoffset.")
        )
        base.Init parent

    override this.OnClose() = ()
    override this.Title = L"offset.visualoffset.result"

type FakePlayfield() =
    inherit StaticWidget(NodeType.None)
    // playfield prep
    let keys = 4

    let column_width = Content.noteskinConfig().ColumnWidth
    let column_spacing = Content.noteskinConfig().ColumnSpacing

    let columnPositions = Array.init keys (fun i -> float32 i * (column_width + column_spacing))
    let playfieldColor = Content.noteskinConfig().PlayfieldColor
    let rotation = Content.Noteskins.noteRotation keys
    let animation = Animation.Counter (Content.noteskinConfig().AnimationFrameTime)
    let timer = Animation.Counter 500.0
    
    let scrollDirectionPos bottom : Rect -> Rect =
        if options.Upscroll.Value then id 
        else fun (r: Rect) -> { r with Top = bottom - r.Bottom; Bottom = bottom - r.Top }

    override this.Init parent =
        let width = Array.mapi (fun i n -> n + column_width) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Content.noteskinConfig().PlayfieldAlignment
        this.Position <-
            { 
                Left = screenAlign %- (width * columnAlign)
                Top = Position.min
                Right = screenAlign %+ (width * (1.0f - columnAlign))
                Bottom = Position.max
            }
        base.Init parent

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        animation.Update elapsedTime
        timer.Update elapsedTime

    override this.Draw() =

        // draw fake playfield
        let scale = float32 options.ScrollSpeed.Value / Gameplay.rate.Value * 1.0f</ms>
        let hitposition = float32 options.HitPosition.Value
        for k in 0 .. (keys - 1) do
            Draw.rect (Rect.Create(this.Bounds.Left + columnPositions.[k], this.Bounds.Top, this.Bounds.Left + columnPositions.[k] + column_width, this.Bounds.Bottom)) playfieldColor
            Draw.quad // receptor
                (
                    Rect.Box(this.Bounds.Left + columnPositions.[k], hitposition, column_width, column_width)
                    |> scrollDirectionPos this.Bounds.Bottom
                    |> Quad.ofRect
                    |> rotation k
                )
                (Color.White |> Quad.colorOf)
                (Sprite.gridUV (animation.Loops, 0) (Content.getTexture "receptor"))

        let mutable y = hitposition + scale * 1.0f<ms> * (500.0f - float32 timer.Time)
        while y < this.Bounds.Height do
            //note in column 0
            Draw.quad // normal note
                (
                    Rect.Box(this.Bounds.Left + columnPositions.[0], y, column_width, column_width)
                    |> scrollDirectionPos this.Bounds.Bottom
                    |> Quad.ofRect
                    |> rotation 0
                )
                (Quad.colorOf Color.White)
                (Sprite.gridUV (animation.Loops, 0) (Content.getTexture "note"))
            y <- y + scale * 500.0f<ms>

    member this.OffsetOfTap : Time =
        let x = 500.0f<ms> - float32 timer.Time * 1.0f<ms>
        if x > 250.0f<ms> then x - 500.0f<ms> else x

type VisualSyncPart1() =
    inherit Menu.Page()

    let fake_playfield = FakePlayfield()

    let taps = ResizeArray<Time>()
    let mutable variance_of_mean = infinityf * 1.0f<ms^2>
    let threshold = 5f<ms^2>

    let mutable complete = false

    let start() =
        Devices.changeVolume 0.0

    let finish(offset) =
        complete <- true
        Devices.changeVolume options.AudioVolume.Value

    let tap_fade = Animation.Fade 0.0f
    let step1_fade = Animation.Fade 1.0f
    let done_button =
        Conditional((fun () -> complete),
            IconButton(
                "Done",
                Icons.ready,
                80.0f,
                ignore,
                Position = Position.Row(700.0f, 80.0f).TrimRight(200.0f).SliceRight(250.0f))
        )

    override this.Init(parent) =
        this 
        |+ fake_playfield
        |* done_button

        base.Init(parent)

        start()

    override this.Draw() =
        base.Draw()

        Text.drawFillB(
            Style.baseFont,
            "Tap when the note reaches the receptor ...",
            this.Bounds.SliceTop(280.0f).SliceBottom(80.0f),
            (Color.FromArgb(step1_fade.Alpha, Color.White), Color.FromArgb(step1_fade.Alpha, Color.Black)),
            Alignment.CENTER)
        Text.drawFillB(
            Style.baseFont,
            "You can use any key",
            this.Bounds.SliceTop(330.0f).SliceBottom(50.0f),
            (Color.FromArgb(step1_fade.Alpha, Color.Silver), Color.FromArgb(step1_fade.Alpha, Color.Black)),
            Alignment.CENTER)

        let progress = min 15.0f (variance_of_mean / threshold)
        Draw.rect (Rect.Create(this.Bounds.CenterX - progress * 15.0f, this.Bounds.Top + 350.0f, this.Bounds.CenterX + progress * 15.0f, this.Bounds.Top + 410.0f)) (Color.FromArgb(tap_fade.Alpha, Colors.pink))
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        tap_fade.Update elapsedTime
    
        if complete then ()
        else
        match Input.consumeAny InputEvType.Press with
        | ValueSome (Key _, t) ->
            let offset = fake_playfield.OffsetOfTap
            taps.Add offset
            tap_fade.Value <- 1.0f

            if taps.Count > 4 then
                let mean = Seq.average taps
                variance_of_mean <- 
                    let sum_of_squares = taps |> Seq.sumBy (fun x -> (x - mean) * (x - mean)) in
                    sum_of_squares / float32 (taps.Count - 1) / float32 taps.Count
                if variance_of_mean < threshold then
                    printfn "Chart: 0 You: %f" mean
                    finish mean
                if taps.Count > 16 then
                    for t in taps do
                        if abs (t - mean) > 10.0f<ms> then sync(fun () -> taps.Remove t |> ignore)
        | _ -> ()

    override this.Title = ""
    override this.OnClose() = ()