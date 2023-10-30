namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open ManagedBass
open Percyqaz.Common

[<AutoOpen>]
module private Helpers =
    let bassError b = ()//if b then () else Logging.Debug("Bass Error: " + Bass.LastError.ToString(), Environment.StackTrace)

type Song =
    {
        ID: int
        Frequency: int // in hz
        Duration: float32<ms>
    }
    static member Default = { ID = 0; Frequency = 1; Duration = 1000.0f<ms> }
    static member FromFile(file: string) =
        //let ID = Bass.CreateStream(file, int64 0, int64 0, BassFlags.Decode); //loads file
        let ID = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan)
        if ID = 0 then 
            Logging.Error("Couldn't load audio track from " + file, Bass.LastError)
            Song.Default
        else
            let d = Bass.ChannelGetInfo ID
            let Duration = Bass.ChannelBytes2Seconds(ID, Bass.ChannelGetLength ID) * 1000.0
            let Frequency = d.Frequency
            //let ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource)
            { ID = ID; Frequency = Frequency; Duration = float32 Duration * 1.0f<ms> }
    member this.Free() = Bass.StreamFree this.ID |> bassError

[<RequireQualifiedAccess>]
type SongFinishAction =
    | LoopFromPreview
    | LoopFromBeginning
    | Wait

module Song = 

    let LEADIN_TIME = 2000.0f<ms>
    
    let mutable load_path : string option = None
    let mutable loading = false

    let mutable nowplaying : Song = Song.Default
    let private timer = new Stopwatch()
    let mutable private timerStart = 0.0f<ms>
    let mutable private channelPlaying = false
    let mutable private rate = 1.0f
    let mutable localOffset = 0.0f<ms>
    let mutable private globalOffset = 0.0f<ms>
    let mutable onFinish = SongFinishAction.Wait
    let mutable private preview_point = 0.0f<ms>
    let mutable private last_note = 0.0f<ms>

    let duration() = nowplaying.Duration

    let time() = rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms>) + timerStart

    let timeWithOffset() = time() + localOffset + globalOffset * rate

    let playing() = timer.IsRunning

    let playFrom(time) =
        timerStart <- time
        if (time >= 0.0f<ms> && time < duration()) then
            channelPlaying <- true
            Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, float <| time / 1000.0f<ms>)) |> bassError
            Bass.ChannelPlay nowplaying.ID |> bassError
        else if channelPlaying then
            Bass.ChannelStop nowplaying.ID |> bassError
            channelPlaying <- false
        timer.Restart()

    let playLeadIn() = playFrom(-LEADIN_TIME * rate)

    let seek(time) =
        if playing() then
            playFrom time
        else
            if (time >= 0.0f<ms> && time < duration()) then
                Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, float <| time / 1000.0f<ms>)) |> bassError
            timer.Reset()
            timerStart <- time

    let pause() =
        let time = time()
        if (time >= 0.0f<ms> && time < duration()) then Bass.ChannelPause nowplaying.ID |> bassError
        timer.Stop()

    let resume() =
        let time = time()
        if (time >= 0.0f<ms> && time < duration()) then Bass.ChannelPlay nowplaying.ID |> bassError
        timer.Start()

    let changeRate(newRate) =
        let didRateChange = rate <> newRate
        let time = time()
        rate <- newRate
        //if (true) then Bass.ChannelSetAttribute(nowplaying.ID, ChannelAttribute.Pitch, -Math.Log(float rate, 2.0) * 12.0) |> bassError
        Bass.ChannelSetAttribute(nowplaying.ID, ChannelAttribute.Frequency, float32 nowplaying.Frequency * rate) |> bassError
        if didRateChange then seek time

    let changeLocalOffset(offset) = localOffset <- offset
    let changeGlobalOffset(offset) = globalOffset <- offset

    let private song_loader =
        { new Async.SwitchService<string option, Song>() with
            override this.Process(path) =
                async {
                    return 
                        match path with
                        | Some p -> Song.FromFile p
                        | None -> Song.Default
                }
            override this.Handle(song) =
                loading <- false
                nowplaying <- song
                changeRate rate
                playFrom preview_point
        }

    let change (path: string option, offset: Time, new_rate: float32, (preview, chart_last_note)) =
        let isDifferentFile = path <> load_path
        load_path <- path
        preview_point <- preview
        last_note <- chart_last_note
        changeLocalOffset offset
        changeRate new_rate
        if isDifferentFile then
            if playing() then pause()
            timerStart <- -infinityf * 1.0f<ms>
            if nowplaying.ID <> 0 then
                nowplaying.Free()
            channelPlaying <- false
            loading <- true
            song_loader.Request(path)

    let update() =

        song_loader.Join()

        let t = time()
        if (t >= 0.0f<ms> && t < nowplaying.Duration && not channelPlaying) then
            channelPlaying <- true
            Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, float <| t / 1000.0f<ms>)) |> bassError
            Bass.ChannelPlay nowplaying.ID |> bassError
        elif t > nowplaying.Duration then
            channelPlaying <- false
            match onFinish with
            | SongFinishAction.LoopFromPreview -> if t >= last_note then playFrom preview_point
            | SongFinishAction.LoopFromBeginning -> if t >= last_note then playFrom 0.0f<ms>
            | SongFinishAction.Wait -> ()

type SoundEffect =
    {
        ID: int
        ChannelID: int
    }
    static member FromStream(source: string, stream: IO.Stream) =
        use ms = new IO.MemoryStream()
        stream.CopyTo ms
        let data = ms.ToArray()
        let id = Bass.SampleLoad(data, 0L, data.Length, 1, BassFlags.SampleOverrideLongestPlaying ||| BassFlags.Prescan)
        if id = 0 then Logging.Error(sprintf "Couldn't load sound effect '%s'" source, Bass.LastError)
        let channel = Bass.SampleGetChannel(id)
        { ID = id; ChannelID = channel }
    member this.Free() = if this.ID <> 0 then Bass.SampleFree this.ID |> bassError
    static member Default = { ID = 0; ChannelID = 0 }

module SoundEffect =

    // todo: if you change audio device sfx stops working
    let play (fx: SoundEffect) (volume: float) =
        Bass.ChannelSetDevice(fx.ChannelID, Bass.CurrentDevice) |> bassError
        Bass.ChannelSetAttribute(fx.ChannelID, ChannelAttribute.Volume, volume * 2.0) |> bassError
        Bass.ChannelPlay(fx.ChannelID, true) |> bassError

type SoundEffect with
    member this.Play() = if this.ID <> 0 then SoundEffect.play this 1.0

module Devices =

    let private fft: float32 array = Array.zeroCreate 1024
    let waveForm: float32 array = Array.zeroCreate 256

    let private updateWaveform(elapsedTime) =
        let lerp_amount = MathF.Pow(0.988f, float32 elapsedTime)
        if Song.playing() then
            Bass.ChannelGetData(Song.nowplaying.ID, fft, int DataFlags.FFT2048) |> ignore
            //algorithm adapted from here
            //https://www.codeproject.com/Articles/797537/Making-an-Audio-Spectrum-analyzer-with-Bass-dll-Cs
            let mutable b0 = 0
            for i in 0 .. 255 do
                let mutable peak = 0.0f
                let mutable b1 = Math.Min(Math.Pow(2.0, float i * 10.0 / 255.0) |> int, 1023)
                if (b1 <= b0) then b1 <- b0 + 1
                while (b0 < b1) do
                    if (peak < fft.[1 + b0]) then peak <- fft.[1 + b0]
                    b0 <- b0 + 1
                let y = Math.Clamp(Math.Sqrt(float peak) * 3.0 * 255.0 - 4.0, 0.0, 255.0) |> float32
                waveForm.[i] <- Percyqaz.Flux.Utils.lerp lerp_amount y waveForm.[i]
        else
            for i in 0 .. 255 do waveForm.[i] <- waveForm.[i] * lerp_amount

    let update(elapsedTime: float) =
        updateWaveform elapsedTime
        Song.update()

    let mutable private defaultDevice = -1
    let mutable private devices = [||]

    let change_volume (fx_volume: float, song_volume: float) = 
        Bass.GlobalSampleVolume <- int (fx_volume * 8000.0) |> max 0
        Bass.GlobalStreamVolume <- int (song_volume * 8000.0) |> max 0

    let private get() =
        devices <-
            seq {
                for i = 1 to Bass.DeviceCount do
                    let ok, info = Bass.GetDeviceInfo i
                    if ok then 
                        if info.IsDefault then defaultDevice <- i
                        yield i, info.Name
            } |> Array.ofSeq

    let list() =
        seq {
            yield -1, "Default"
            yield! devices
        } |> Array.ofSeq

    let change(index: int) =
        try 
            let id = if index = -1 then defaultDevice else fst devices.[index - 1]
            Bass.CurrentDevice <- id
            if Song.nowplaying.ID <> 0 then Bass.ChannelSetDevice(Song.nowplaying.ID, id) |> bassError
        with err -> Logging.Error(sprintf "Error switching to audio output %i" index, err)

    let init(device: int) =
        get()
        for (i, name) in devices do
            // https://github.com/ppy/osu/issues/3800
            Bass.Configure(Configuration.DevNonStop, true) |> bassError
            Bass.Configure(Configuration.DevicePeriod, 2) |> bassError
            Bass.Configure(Configuration.DeviceBufferLength, 4) |> bassError
            Bass.Init i |> bassError
        change device
        Bass.GlobalStreamVolume <- 0