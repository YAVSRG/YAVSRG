namespace Interlude.Features.Offset

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Interlude.Options
open Interlude.UI
open Interlude.Features

type AudioSync(chart: Chart, when_done: unit -> unit) =
    inherit StaticContainer(NodeType.Leaf)

    let chart_mspb, chart_por =
        let previewTime = chart.Header.PreviewTime + 200.0f<ms>
        try
            let (por, (_, mspb)) = chart.BPM.GetPointAt(previewTime) in (mspb * 1.0f<beat>, por %% (mspb * 1.0f<beat>))
        with err ->
            let (por, (_, mspb)) = chart.BPM.First.Value in (mspb * 1.0f<beat>, por %% (mspb * 1.0f<beat>))

    let taps = ResizeArray<Time>()
    let mutable variance_of_mean = infinityf * 1.0f<ms^2>
    let threshold = 5f<ms^2>

    let mutable complete = false

    let mutable rates = [0.9f; 1.0f]
    let mutable rate = 0.8f

    let results = ResizeArray<float32 * Time>()

    let start() =
        Song.changeRate rate
        Song.playFrom(chart.Header.PreviewTime)

    let next_rate(offset) =
        results.Add((rate, offset))
        if rates <> [] then
            rate <- List.head rates
            rates <- List.tail rates
            variance_of_mean <- infinityf * 1.0f<ms^2>
            taps.Clear()
            start()
        else 
            complete <- true
            Song.changeRate Gameplay.rate.Value
            let mutable x = 0.0f
            let mutable y = 0.0f<ms>
            let mutable xx = 0.0f
            let mutable xy = 0.0f<ms>
            let n = float32 results.Count
            for (rate, offset) in results do
                printfn "Rate: %f Offset: %f" rate offset
                x <- x + rate
                y <- y + offset
                xx <- xx + rate * rate
                xy <- xy + rate * offset
            let gradient = (n * xy - x * y) / (n * xx - x * x)
            let intercept = (y - gradient * x) / n

            printfn "Est: Offset = %f * Rate + %f" gradient intercept

    let tap_fade = Animation.Fade 0.0f
    let step1_fade = Animation.Fade 1.0f
    let step2_fade = Animation.Fade 0.0f
    let done_button =
        Conditional((fun () -> complete),
            IconButton(
                "Done",
                Icons.ready,
                80.0f,
                when_done,
                Position = Position.Row(700.0f, 80.0f).TrimRight(200.0f).SliceRight(250.0f))
        )

    override this.Init(parent) =
        this |* done_button

        base.Init(parent)

        start()

    override this.Draw() =
        base.Draw()

        Text.drawFillB(
            Style.baseFont,
            "Tap to the beat ...",
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
            let raw_song_time = (t - 1.0f<ms> * float32 options.AudioOffset.Value * rate - Song.localOffset)
            let offset = (raw_song_time % chart_mspb) - chart_por
            taps.Add offset
            tap_fade.Value <- 1.0f

            if taps.Count > 4 then
                let mean = Seq.average taps
                variance_of_mean <- 
                    let sum_of_squares = taps |> Seq.sumBy (fun x -> (x - mean) * (x - mean)) in
                    sum_of_squares / float32 (taps.Count - 1) / float32 taps.Count
                if variance_of_mean < threshold then
                    printfn "Chart: %f You: %f" chart_por mean
                    next_rate mean
                else 
                    printfn "%f, mean %f" offset mean
                    printfn "%f > %f" variance_of_mean threshold
                if taps.Count > 16 then
                    for t in taps do
                        if abs (t - mean) > 10.0f<ms> then sync(fun () -> taps.Remove t |> ignore)
        | _ -> ()