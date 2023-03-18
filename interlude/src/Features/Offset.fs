namespace Interlude.Features

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

module private LoadWaveform =
    
    let loader =
        { new Async.SwitchService<string, Waveform.Waveform>()
            with override this.Handle(path) = async { return Waveform.generate path }
        }

type WaveformRender(fade: Animation.Fade) =
    inherit StaticContainer(NodeType.None)
    
    let mutable waveform : Waveform.Waveform = { MsPerPoint = 1.0f<ms>; Points = Array.zeroCreate 0 }

    do
        match Gameplay.Chart.current with
        | Some c -> LoadWaveform.loader.Request(c.AudioPath, fun wf -> sync(fun () -> waveform <- wf))
        | None -> ()

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
        let mutable por = (this.PointOfReference + 1.0f<ms> * float32 options.AudioOffset.Value * Gameplay.rate.Value)
        let left = time - rel / scale
        let right = time + (rel / 3.0f) / scale
        por <- por + this.MsPerBeat * ceil ((left - por) / this.MsPerBeat)
        while por < right do
            let x = this.Bounds.Left + rel + (por - time) * scale
            Draw.rect (Rect.Create (x, this.Bounds.Top, x + 2.0f, this.Bounds.Bottom)) (Color.FromArgb(fade.Alpha, Color.Red))
            por <- por + this.MsPerBeat

type GlobalSync(chart: Chart) =
    inherit StaticContainer(NodeType.Leaf)

    let chart_mspb, chart_por =
        let (por, (_, mspb)) = chart.BPM.First.Value in (mspb * 1.0f<beat>, por %% (mspb * 1.0f<beat>))

    let mutable step2_completion = 0
    let mutable deviation = 60.0f<ms>
    let step1_threshold = 12.0f<ms>
    let step2_threshold = 5.0f<ms>

    let mutable step = 0

    let taps = ResizeArray<Time>()
    let tap_fade = Animation.Fade 0.0f
    let step1_fade = Animation.Fade 0.0f
    let step2_fade = Animation.Fade 0.0f
    let waveform_fade = Animation.Fade 0.0f
    let step3_fade = Animation.Fade 0.0f
    let waveform = WaveformRender(waveform_fade, Position = { Position.Default with Top = 0.5f %- 100.0f; Bottom = 0.5f %+ 100.0f })
    let offset_changer =
        Conditional((fun () -> step = 3),
            PrettySetting(
                "system.audiooffset",
                Slider(options.AudioOffset |> Setting.trigger (fun v -> Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>)), 0.001f)
            ).Pos(700.0f)
        )
    let done_button =
        Conditional((fun () -> step = 3),
            IconButton(
                "Done",
                Interlude.UI.Icons.ready,
                80.0f,
                Menu.Back,
                Position = Position.Row(700.0f, 80.0f).TrimRight(200.0f).SliceRight(250.0f))
        )

    let step_1() =
        step <- 1
        taps.Clear()
        step1_fade.Target <- 1.0f
        step2_fade.Target <- 0.0f
        step3_fade.Target <- 0.0f
        waveform_fade.Target <- 0.0f

    let step_2(por, mspb) =
        step <- 2
        waveform.PointOfReference <- por
        waveform.MsPerBeat <- mspb
        step1_fade.Target <- 0.0f
        step2_fade.Target <- 1.0f
        waveform_fade.Target <- 1.0f

    let step_3() =
        step <- 3
        step2_fade.Target <- 0.0f
        step3_fade.Target <- 1.0f
        offset_changer.Update(0.0, true)

    override this.Init(parent) =
        this
        |+ offset_changer
        |+ done_button
        |* waveform

        base.Init(parent)

        step_1()

    override this.Draw() =
        base.Draw()

        // step 1
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

        let w, color = 
            if taps.Count <= 12 then float32 (14 - taps.Count) / 12.0f * 100.0f, Color.White
            else deviation / step1_threshold * 25.0f, Color.FromArgb(200, 200, 255)
        Draw.rect (Rect.Create(this.Bounds.CenterX - w, this.Bounds.Top + 350.0f, this.Bounds.CenterX + w, this.Bounds.CenterY - 120.0f)) (Color.FromArgb(tap_fade.Alpha, color))

        // step 2
        Text.drawFillB(
            Style.baseFont,
            "Keep tapping to the beat until the lines stop moving",
            this.Bounds.SliceTop(280.0f).SliceBottom(80.0f),
            (Color.FromArgb(step2_fade.Alpha, Color.White), Color.FromArgb(step2_fade.Alpha, Color.Black)),
            Alignment.CENTER)
        Text.drawFillB(
            Style.baseFont,
            "If the red lines keep jumping around, try another song",
            this.Bounds.SliceTop(330.0f).SliceBottom(50.0f),
            (Color.FromArgb(step2_fade.Alpha, Color.Silver), Color.FromArgb(step2_fade.Alpha, Color.Black)),
            Alignment.CENTER)

        // step 3
        Text.drawFillB(
            Style.baseFont,
            "Adjust your offset to align with the peaks",
            this.Bounds.SliceTop(280.0f).SliceBottom(80.0f),
            (Color.FromArgb(step3_fade.Alpha, Color.White), Color.FromArgb(step3_fade.Alpha, Color.Black)),
            Alignment.CENTER)
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        tap_fade.Update elapsedTime
        step1_fade.Update elapsedTime
        step2_fade.Update elapsedTime
        step3_fade.Update elapsedTime
    
        if step = 3 then ()
        else
        match Input.consumeAny InputEvType.Press with
        | ValueSome (Key _, t) ->
            let raw_song_time = (t - 1.0f<ms> * float32 options.AudioOffset.Value * Gameplay.rate.Value - Song.localOffset)
            taps.Add raw_song_time
            tap_fade.Value <- 1.0f

            if taps.Count > 12 then
                if step = 1 then
                    let deltas = taps |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)
                    let average_deviation = (deltas |> Seq.sumBy(fun x -> abs (x - chart_mspb))) / float32 taps.Count
                    deviation <- average_deviation
                    let acceptable = average_deviation < step1_threshold
                    if acceptable then 
                        let mutable w = 0.0f
                        let mutable t = 0.0f<ms>
                        for i = 1 to taps.Count - 1 do
                            t <- t + (taps[i] % chart_mspb) * 0.5f
                            t <- t + (taps[i - 1] % chart_mspb) * 0.5f
                            w <- w + 1.0f
                        let por = t / w
                        step_2(por, chart_mspb)
                elif step = 2 then
                    let t = taps.[taps.Count - 1]
                    let diff = (t % chart_mspb) - waveform.PointOfReference
                    waveform.PointOfReference <- waveform.PointOfReference + diff * 0.3f
                    step2_completion <- step2_completion + 1
                    if Time.Abs diff * 0.3f > step2_threshold then step2_completion <- 0
                    if step2_completion = 5 then step_3()

            if taps.Count > 13 then taps.RemoveAt(0)
        | _ -> ()

type TileButton(body: Callout, onclick: unit -> unit) =
    inherit StaticContainer(NodeType.Button (onclick))

    let body_height = Callout.measure body

    member this.Height = body_height + this.Margin * 2.0f
    member val Active = false with get, set
    member val Disabled = false with get, set
    member val Margin = 20.0f with get, set

    override this.Init(parent) =
        this |* Clickable.Focus(this)
        base.Init(parent)

    override this.Draw() =
        let color, dark = 
            if this.Disabled then Colors.grey1, false
            elif this.Active then Colors.yellow, true
            elif this.Focused then Colors.pink, false
            else Colors.grey1, false
        Draw.rect this.Bounds (Color.FromArgb(180, color))
        Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color
        Callout.draw (this.Bounds.Left + this.Margin, this.Bounds.Top + this.Margin, body_height, dark, body)

type OffsetPage(chart: Chart) as this =
    inherit Page()

    do 
        let goffset_tile = 
            TileButton(
                Callout.Small.Body("Compensate for hardware delay\nUse this if all songs are offsync").Title("Global offset").Icon(Icons.connected),
                ignore
            )

        goffset_tile.Position <- Position.Box(0.33f, 0.1f, 600.0f, goffset_tile.Height)

        this.Content(
            column()
            |+ PrettyButton("offset.globaloffset", 
                fun () -> 
                    goffset_tile.Active <- true
                    { new Page() with
                        override this.Init(parent) =
                            this.Content(GlobalSync chart)
                            base.Init parent
                        override this.OnClose() = ()
                        override this.Title = N"offset.globaloffset"
                    }.Show()).Pos(200.0f)
            |+ goffset_tile
        )

    override this.Title = N"offset"
    override this.OnClose() = ()