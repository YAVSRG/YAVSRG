namespace Interlude

open System
open System.Diagnostics
open ManagedBass
open ManagedBass.Fx
open Prelude.Common

module Audio = 

    let bassError b =
        if b then () else Logging.Debug("Bass Error: " + Bass.LastError.ToString()) System.Environment.StackTrace

    type Track = {
        ID: int //id used by Bass
        Frequency: int //frequency the file is encoded in, so it knows how to up and downrate the audio correctly
        Duration: float //duration of the song so it doesn't have to be tediously recalculated in case of mp3 and stuff
    } with
        static member Default = { ID = 0; Frequency = 1; Duration = 1000.0 }
        static member FromFile(file: string) =
            //let ID = Bass.CreateStream(file, int64 0, int64 0, BassFlags.Decode); //loads file
            let ID = Bass.CreateStream(file); //loads file
            if ID = 0 then 
                Logging.Error("Couldn't load audio track from " + file) <| Bass.LastError.ToString()
                Track.Default
            else
                let d = Bass.ChannelGetInfo(ID) //asks bass for info about the track
                let Duration = Bass.ChannelBytes2Seconds(ID, Bass.ChannelGetLength(ID)) * 1000.0
                let Frequency = d.Frequency
                //let ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource)
                { ID = ID; Frequency = Frequency; Duration = Duration }
        member this.Dispose() =
            Bass.StreamFree(this.ID) |> bassError

    type TrackFinishBehaviour =
    | Loop of double
    | Wait
    | Action of (unit -> unit)

    let private LEADIN_TIME = 3000.0
    
    let mutable private nowplaying: Track = Track.Default
    let private fft: float32 array = Array.zeroCreate 1024
    let waveForm: float32 array = Array.zeroCreate 256
    let private timer = new Stopwatch()
    let mutable private timerStart = 0.0
    let mutable private channelPlaying = false
    let mutable private rate = 1.0
    let mutable private localOffset = 0.0
    let mutable globalOffset = 0.0

    let audioDuration() = nowplaying.Duration

    let time() = float timer.ElapsedMilliseconds + timerStart

    let timeWithOffset() = time() + localOffset + globalOffset * rate

    let playing() = timer.IsRunning

    let playFrom(time) =
        timerStart <- time
        if (time > 0.0 && time < audioDuration()) then
            channelPlaying <- true
            Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, time / 1000.0)) |> bassError
            Bass.ChannelPlay(nowplaying.ID) |> bassError
        else
            Bass.ChannelPause(nowplaying.ID) |> bassError
            channelPlaying <- false
        timer.Restart()

    let playLeadIn() = playFrom(-LEADIN_TIME)

    let seek(time) =
        timerStart <- time
        if (time > 0.0 && time < audioDuration()) then
            channelPlaying <- true
            Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, time / 1000.0)) |> bassError
        else
            Bass.ChannelPause(nowplaying.ID) |> bassError
            channelPlaying <- false
        timer.Reset()

    let pause() =
        Bass.ChannelPause(nowplaying.ID) |> bassError
        timer.Stop()

    let resume() =
        //possible todo: should seek to where timer thinks we are to reduce drift
        Bass.ChannelPlay(nowplaying.ID) |> bassError
        timer.Start()

    let private updateWaveform() =
        if playing() then
            Bass.ChannelGetData(nowplaying.ID, fft, int DataFlags.FFT2048) |> ignore
            //algorithm adapted from here
            //https://www.codeproject.com/Articles/797537/Making-an-Audio-Spectrum-analyzer-with-Bass-dll-Cs
            let mutable b0 = 0
            for i in 0..255 do
                let mutable peak = 0.0f
                let mutable b1 = Math.Min(Math.Pow(2.0, float i * 10.0 / 255.0) |> int, 1023)
                if (b1 <= b0) then b1 <- b0 + 1
                while (b0 < b1) do
                    if (peak < fft.[1 + b0]) then peak <- fft.[1 + b0]
                    b0 <- b0 + 1
                let y = Math.Clamp(Math.Sqrt(float peak) * 3.0 * 255.0 - 4.0, 0.0, 255.0) |> float32
                waveForm.[i] <- waveForm.[i] * 0.9f + y * 0.1f
        else
            for i in 0..255 do waveForm.[i] <- waveForm.[i] * 0.9f

    let update() =
        updateWaveform()

        let t = time()
        if (t > 0.0 && t < nowplaying.Duration && not channelPlaying) then
            channelPlaying <- true
            Bass.ChannelSetPosition(nowplaying.ID, Bass.ChannelSeconds2Bytes(nowplaying.ID, t / 1000.0)) |> bassError
            Bass.ChannelPlay(nowplaying.ID) |> bassError
        elif (t > nowplaying.Duration) then
            channelPlaying <- false
            //todo: handle action for when track is complete here

    let changeRate(newRate) =
        rate <- newRate;
        //if (PREVENT_PITCH_CHANGE) then Bass.ChannelSetAttribute(nowplaying.ID, ChannelAttribute.Pitch, -Math.Log(rate, 2.0) * 12.0) |> ignore
        Bass.ChannelSetAttribute(nowplaying.ID, ChannelAttribute.Frequency, float nowplaying.Frequency * rate) |> bassError

    let changeTrack(path, offset, rate) =
        if playing() then pause()
        if nowplaying.ID <> 0 then
            nowplaying.Dispose()
        nowplaying <- Track.FromFile(path)
        localOffset <- offset
        changeRate(rate)

    let init() =
        Bass.Init() |> bassError
        Bass.GlobalStreamVolume <- int (0.05 * 10000.0)