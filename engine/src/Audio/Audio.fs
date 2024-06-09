namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open ManagedBass
open Percyqaz.Common

[<AutoOpen>]
module private Helpers =
    let display_bass_error b = () // if b then () else Logging.Debug("Bass Error: " + Bass.LastError.ToString(), Environment.StackTrace)

    let mutable internal current_device = -1

type Song =
    {
        ID: int
        Frequency: int // in hz
        Duration: float32<ms>
    }
    static member Default =
        {
            ID = 0
            Frequency = 1
            Duration = 1000.0f<ms>
        }

    static member FromFile(file: string) =
        let ID = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan (* ||| BassFlags.Decode *))

        if ID = 0 then
            Logging.Error("Couldn't load audio track from " + file, Bass.LastError)
            Song.Default
        else
            let d = Bass.ChannelGetInfo ID
            let Duration = Bass.ChannelBytes2Seconds(ID, Bass.ChannelGetLength ID) * 1000.0
            let Frequency = d.Frequency
            Bass.ChannelSetDevice(ID, current_device) |> display_bass_error
            (* let ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource) *)
            {
                ID = ID
                Frequency = Frequency
                Duration = float32 Duration * 1.0f<ms>
            }

    member this.Free() =
        Bass.StreamFree this.ID |> display_bass_error

[<RequireQualifiedAccess>]
type SongFinishAction =
    | LoopFromPreview
    | LoopFromBeginning
    | Wait
    | Custom of (unit -> unit)

[<RequireQualifiedAccess>]
type SongLoadAction =
    | PlayFromPreview
    | PlayFromBeginning
    | Wait

module Song =

    let LEADIN_TIME = 2000.0f<ms>

    let mutable load_path: string option = None
    let mutable loading = false

    let mutable now_playing: Song = Song.Default
    let private timer = new Stopwatch()
    let mutable private timer_start = 0.0f<ms>
    let mutable private channel_playing = false
    let mutable private rate = 1.0f
    let mutable _local_offset = 0.0f<ms>
    let mutable private _global_offset = 0.0f<ms>
    let mutable on_finish = SongFinishAction.Wait
    let mutable private preview_point = 0.0f<ms>
    let mutable private last_note = 0.0f<ms>

    let duration () = now_playing.Duration

    let time () =
        rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms>) + timer_start

    let time_with_offset () =
        time () + _local_offset + _global_offset * rate

    let playing () = timer.IsRunning

    let play_from (time) =
        timer_start <- time

        if time >= 0.0f<ms> && time < duration () then
            channel_playing <- true

            Bass.ChannelSetPosition(
                now_playing.ID,
                Bass.ChannelSeconds2Bytes(now_playing.ID, float <| time / 1000.0f<ms>)
            )
            |> display_bass_error

            Bass.ChannelPlay now_playing.ID |> display_bass_error
        else if channel_playing then
            Bass.ChannelStop now_playing.ID |> display_bass_error
            channel_playing <- false

        timer.Restart()

    let play_leadin () = play_from (-LEADIN_TIME * rate)

    let seek (time) =
        if playing () then
            play_from time
        else
            if time >= 0.0f<ms> && time < duration () then
                Bass.ChannelSetPosition(
                    now_playing.ID,
                    Bass.ChannelSeconds2Bytes(now_playing.ID, float <| time / 1000.0f<ms>)
                )
                |> display_bass_error
            else if channel_playing then
                Bass.ChannelStop now_playing.ID |> display_bass_error
                channel_playing <- false

            timer.Reset()
            timer_start <- time

    let pause () =
        let time = time ()

        if time >= 0.0f<ms> && time < duration () then
            Bass.ChannelPause now_playing.ID |> display_bass_error

        timer.Stop()

    let resume () =
        let time = time ()

        if time >= 0.0f<ms> && time < duration () then
            Bass.ChannelPlay now_playing.ID |> display_bass_error

        timer.Start()

    let change_rate (new_rate) =
        let rate_changed = rate <> new_rate
        let time = time ()
        rate <- new_rate
        (* Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Pitch, -Math.Log(float rate, 2.0) * 12.0)
        |> display_bass_error *)
        Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency * rate)
        |> display_bass_error

        if rate_changed then
            seek time

    let set_local_offset (offset) = _local_offset <- offset
    let set_global_offset (offset) = _global_offset <- offset

    let private song_loader =
        { new Async.SwitchService<string option * SongLoadAction * bool, Song * SongLoadAction * bool>() with
            override this.Process((path, after_load, song_playing)) =
                async {
                    return
                        match path with
                        | Some p -> Song.FromFile p, after_load, song_playing
                        | None -> Song.Default, after_load, song_playing
                }

            override this.Handle((song, after_load: SongLoadAction, song_playing: bool)) =
                loading <- false
                now_playing <- song
                change_rate rate

                match after_load with
                | SongLoadAction.PlayFromPreview ->
                    (if song_playing then play_from else seek) preview_point
                | SongLoadAction.PlayFromBeginning ->
                    (if song_playing then play_from else seek) 0.0f<ms>
                | SongLoadAction.Wait -> ()
        }

    let change (path: string option, offset: Time, new_rate: float32, (preview: Time, chart_last_note: Time), after_load: SongLoadAction) =
        let song_was_playing = playing() || load_path = None
        let path_changed = path <> load_path
        load_path <- path
        preview_point <- preview
        last_note <- chart_last_note
        set_local_offset offset
        change_rate new_rate

        if path_changed then
            if song_was_playing then
                pause ()

            timer_start <- -infinityf * 1.0f<ms>

            if now_playing.ID <> 0 then
                now_playing.Free()

            channel_playing <- false
            loading <- true
            song_loader.Request(path, after_load, song_was_playing)

    let update () =

        song_loader.Join()

        let t = time ()

        if playing () && t >= 0.0f<ms> && t < now_playing.Duration && not channel_playing then
            channel_playing <- true

            Bass.ChannelSetPosition(now_playing.ID, Bass.ChannelSeconds2Bytes(now_playing.ID, float <| t / 1000.0f<ms>))
            |> display_bass_error

            Bass.ChannelPlay now_playing.ID |> display_bass_error
        elif t > now_playing.Duration then
            match on_finish with
            | SongFinishAction.LoopFromPreview ->
                if t >= last_note then
                    play_from preview_point
            | SongFinishAction.LoopFromBeginning ->
                if t >= last_note then
                    play_from 0.0f<ms>
            | SongFinishAction.Wait -> ()
            | SongFinishAction.Custom action ->
                if channel_playing then
                    action ()

            channel_playing <- false

type SoundEffect =
    {
        ID: int
        ChannelID: int
    }
    static member FromStream(source: string, stream: IO.Stream) =
        use ms = new IO.MemoryStream()
        stream.CopyTo ms
        let data = ms.ToArray()

        let id =
            Bass.SampleLoad(data, 0L, data.Length, 1, BassFlags.SampleOverrideLongestPlaying ||| BassFlags.Prescan)

        if id = 0 then
            Logging.Error(sprintf "Couldn't load sound effect '%s'" source, Bass.LastError)

        let channel = Bass.SampleGetChannel(id)
        { ID = id; ChannelID = channel }

    member this.Free() =
        if this.ID <> 0 then
            Bass.SampleFree this.ID |> display_bass_error

    static member Default = { ID = 0; ChannelID = 0 }

module SoundEffect =

    // todo: if you change audio device sfx stops working
    let play (fx: SoundEffect) (volume: float) =
        Bass.ChannelSetDevice(fx.ChannelID, Bass.CurrentDevice) |> display_bass_error

        Bass.ChannelSetAttribute(fx.ChannelID, ChannelAttribute.Volume, volume * 2.0)
        |> display_bass_error

        Bass.ChannelPlay(fx.ChannelID, true) |> display_bass_error

type SoundEffect with
    member this.Play() =
        if this.ID <> 0 then
            SoundEffect.play this 1.0

module Devices =

    let private fft: float32 array = Array.zeroCreate 1024
    let waveform: float32 array = Array.zeroCreate 256

    let private update_waveform (elapsed_ms) =
        let lerp_amount = MathF.Pow(0.988f, float32 elapsed_ms)

        if Song.playing () then
            Bass.ChannelGetData(Song.now_playing.ID, fft, int DataFlags.FFT2048) |> ignore
            //algorithm adapted from here
            //https://www.codeproject.com/Articles/797537/Making-an-Audio-Spectrum-analyzer-with-Bass-dll-Cs
            let mutable b0 = 0

            for i in 0..255 do
                let mutable peak = 0.0f
                let mutable b1 = Math.Min(Math.Pow(2.0, float i * 10.0 / 255.0) |> int, 1023)

                if (b1 <= b0) then
                    b1 <- b0 + 1

                while (b0 < b1) do
                    if (peak < fft.[1 + b0]) then
                        peak <- fft.[1 + b0]

                    b0 <- b0 + 1

                let y = Math.Clamp(Math.Sqrt(float peak) * 3.0 * 255.0 - 4.0, 0.0, 255.0) |> float32
                waveform.[i] <- Percyqaz.Flux.Utils.lerp lerp_amount y waveform.[i]
        else
            for i in 0..255 do
                waveform.[i] <- waveform.[i] * lerp_amount

    let update (elapsed_ms: float) =
        update_waveform elapsed_ms
        Song.update ()

    let mutable private default_device = -1
    let mutable private devices = [||]

    let change_volume (fx_volume: float, song_volume: float) =
        Bass.GlobalSampleVolume <- int (fx_volume * 8000.0) |> max 0
        Bass.GlobalStreamVolume <- int (song_volume * 8000.0) |> max 0

    let private get () =
        devices <-
            seq {
                for i = 1 to Bass.DeviceCount do
                    let ok, info = Bass.GetDeviceInfo i

                    if ok then
                        if info.IsDefault then
                            default_device <- i

                        yield i, info.Name
            }
            |> Array.ofSeq

    let list () =
        seq {
            yield -1, "Default"
            yield! devices
        }
        |> Array.ofSeq

    let change (index: int) =
        try
            let id =
                if index = -1 then
                    default_device
                else
                    fst devices.[index - 1]

            Bass.CurrentDevice <- id

            if Song.now_playing.ID <> 0 then
                Bass.ChannelSetDevice(Song.now_playing.ID, id) |> display_bass_error

            current_device <- id
        with err ->
            Logging.Error(sprintf "Error switching to audio output %i" index, err)

    let init (device: int) =
        get ()

        for (i, name) in devices do
            // https://github.com/ppy/osu/issues/3800
            Bass.Configure(Configuration.DevNonStop, true) |> display_bass_error
            Bass.Configure(Configuration.DevicePeriod, 2) |> display_bass_error
            Bass.Configure(Configuration.DeviceBufferLength, 4) |> display_bass_error
            Bass.Init i |> display_bass_error

        change device
        Bass.GlobalStreamVolume <- 0
