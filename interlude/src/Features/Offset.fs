namespace Interlude.Features

open Percyqaz.Common
open Prelude.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Interlude.Options
open Interlude.UI.Menu

type WaveformRender(fade: Animation.Fade) =
    inherit StaticContainer(NodeType.None)
    
    let waveform = 
        try
            let audioPath = Interlude.Features.Gameplay.Chart.current.Value.AudioPath
            Waveform.generate audioPath
        with err -> Logging.Error("Waveform error", err); { MsPerPoint = 1.0f<ms>; Points = Array.zeroCreate 0 }

    member val PointOfReference = 1.0f<ms> with get, set
    member val MsPerBeat = 1.0f<ms> with get, set

    override this.Update(elapsedTime, moved) =
        fade.Update elapsedTime
        base.Update(elapsedTime, moved)

    override this.Draw() =
        if fade.Alpha <= 0 then () else
        let scale = 0.25f</ms>
        let time = Song.time()
        let mutable x = this.Bounds.Left
        let mutable i = int <| time / waveform.MsPerPoint - this.Bounds.Width * 0.75f / scale / waveform.MsPerPoint
        let h = this.Bounds.Height / 2.0f
        let points = waveform.Points
        while x < this.Bounds.Right && i < points.Length do
            if i >= 0 then
                Draw.rect (Rect.Create(x, this.Bounds.CenterY - h * points.[i].Left, x + 1.0f<ms> * scale, this.Bounds.CenterY + h * points.[i].Right)) (Color.FromArgb(fade.Alpha / 2, 200, 200, 255))
            i <- i + 1
            x <- x + waveform.MsPerPoint * scale
        Draw.rect (this.Bounds.TrimRight(this.Bounds.Width * 0.25f).SliceRight(5.0f)) (Color.FromArgb(fade.Alpha, Color.White))
        let rel = this.Bounds.Width * 0.75f
        let mutable por = (this.PointOfReference + 1.0f<ms> * float32 options.AudioOffset.Value * Gameplay.rate.Value) % this.MsPerBeat
        let left = time - rel / scale
        let right = time + (rel / 3.0f) / scale
        por <- por + this.MsPerBeat * ceil ((left - por) / this.MsPerBeat)
        while por < right do
            let x = this.Bounds.Left + rel + (por - time) * scale
            Draw.rect (Rect.Create (x, this.Bounds.Top, x + 2.0f, this.Bounds.Bottom)) (Color.FromArgb(fade.Alpha, Color.Red))
            por <- por + this.MsPerBeat

type Progress =
    | Waiting_For_Taps = 0uy
    | Keep_Going = 1uy
    | Show_Waveform = 2uy
    | Confirm_Global_Sync = 3uy

type OffsetPage() as this =
    inherit Page()

    let chart_mspb, chart_por =
        match Gameplay.Chart.current with
        | Some c -> let (por, (_, mspb)) = c.BPM.First.Value in (mspb * 1.0f<beat>, por)
        | None -> 500.0f<ms>, 0.0f<ms>

    let mutable deviation = 60.0f<ms>
    let threshold = 12.0f<ms>

    let mutable state = Progress.Waiting_For_Taps
    let mutable try_again = false

    let taps = ResizeArray<Time>()
    let tap_fade = Animation.Fade 0.0f
    let waveform_fade = Animation.Fade 0.0f
    let waveform = WaveformRender(waveform_fade, Position = Position.SliceBottom(500.0f).TrimBottom(300.0f))

    do 
        this.Content(
            column()
            |+ PrettySetting("system.audiooffset", Slider(options.AudioOffset |> Setting.trigger (fun v -> Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>)), 0.005f)).Pos(800.0f)
            |+ Text("Tap to the beat ...", Align = Alignment.CENTER, Position = Position.Row(200.0f, 80.0f))
            |+ Text((fun () -> if state = Progress.Keep_Going then "Keep going ..." elif try_again then "A bit more ..." else  ""), Align = Alignment.CENTER, Position = Position.Row(300.0f, 80.0f))
            |+ waveform
        )

    override this.Draw() =
        base.Draw()

        let w = deviation / threshold * 25.0f
        Draw.rect (Rect.Create(this.Bounds.CenterX - w, this.Bounds.Top + 400.0f, this.Bounds.CenterX + w, this.Bounds.Top + 480.0f)) (Color.FromArgb(tap_fade.Alpha, Color.White))
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        tap_fade.Update elapsedTime
    
        match Input.consumeAny InputEvType.Press with
        | ValueSome (Key _, t) ->
            let raw_song_time = (t - 1.0f<ms> * float32 options.AudioOffset.Value * Gameplay.rate.Value - Song.localOffset)
            taps.Add raw_song_time
            tap_fade.Value <- 1.0f

            if taps.Count > 12 then
                if state < Progress.Show_Waveform then
                    let deltas = taps |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)
                    let average_deviation = (deltas |> Seq.sumBy(fun x -> abs (x - chart_mspb))) / float32 taps.Count
                    deviation <- average_deviation
                    let acceptable = average_deviation < threshold
                    if not acceptable then try_again <- true; state <- Progress.Waiting_For_Taps
                    else 
                        state <- Progress.Show_Waveform
                        try_again <- false
                        waveform_fade.Target <- 1.0f
                        let mutable w = 0.0f
                        let mutable t = 0.0f<ms>
                        for i = 1 to taps.Count - 1 do
                            t <- t + (taps[i] % chart_mspb) * 0.5f
                            t <- t + (taps[i - 1] % chart_mspb) * 0.5f
                            w <- w + 1.0f
                        let por = t / w
                        printfn "Your POR: %f" (por % chart_mspb)
                        printfn "Chart POR: %f" (chart_por % chart_mspb)
                        waveform.PointOfReference <- por
                        waveform.MsPerBeat <- chart_mspb
                else
                    let t = taps.[taps.Count - 1]
                    let diff = (t % chart_mspb) - waveform.PointOfReference
                    if Time.Abs diff > 2.0f<ms> then waveform.PointOfReference <- waveform.PointOfReference + diff * 0.3f

            if taps.Count > 13 then taps.RemoveAt(0)
            elif taps.Count > 3 && state < Progress.Keep_Going then state <- Progress.Keep_Going
        | _ -> ()

    override this.Title = N"offset"
    override this.OnClose() = ()