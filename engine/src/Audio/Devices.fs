namespace Percyqaz.Flux.Audio

open System
open ManagedBass
open Percyqaz.Common

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
                waveform.[i] <- lerp lerp_amount y waveform.[i]
        else
            for i in 0..255 do
                waveform.[i] <- waveform.[i] * lerp_amount

    let update (elapsed_ms: float) =
        update_waveform elapsed_ms
        Song.update elapsed_ms

    let mutable private default_device = -1
    let mutable private devices = [||]

    let change_volume (fx_volume: float, song_volume: float) =
        Bass.GlobalSampleVolume <- int (fx_volume * 8000.0) |> max 0
        Bass.GlobalStreamVolume <- int (song_volume * 8000.0) |> max 0

    let private current_device_changed_ev = Event<unit>()
    let current_device_changed = current_device_changed_ev.Publish

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

            current_device_changed_ev.Trigger()

        with err ->
            Logging.Error(sprintf "Error switching to audio output %i" index, err)

    let debug_info() : string =
        
        match Bass.GetInfo() with
        | false, _ -> sprintf "-- AUDIO DEBUG INFO --\nBass Version: %O\nOther details could not be retrieved"  Bass.Version
        | true, info ->
            sprintf 
                """-- AUDIO DEBUG INFO --
BASS Version: %O
Estimated latency: %i
Min buffer length: %i
DirectSound Version: %i"""
                Bass.Version
                info.Latency
                info.MinBufferLength
                info.DSVersion

    let init (device: int, device_period: int, device_buffer_length: int) =
        get ()

        // https://github.com/ppy/osu/issues/3800
        Bass.Configure(Configuration.DevNonStop, true) |> display_bass_error
        Bass.Configure(Configuration.DevicePeriod, device_period) |> display_bass_error
        Bass.Configure(Configuration.DeviceBufferLength, device_buffer_length) |> display_bass_error
        Bass.Configure(Configuration.PlaybackBufferLength, 100) |> display_bass_error
        Bass.Configure(Configuration.UpdatePeriod, 5) |> display_bass_error

        for (i, name) in devices do
            Bass.Init(i, Flags = DeviceInitFlags.Latency) |> display_bass_error

        change device
        Bass.GlobalStreamVolume <- 0
