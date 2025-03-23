namespace Percyqaz.Flux.Audio

open System
open ManagedBass
open Percyqaz.Common

type Device =
    {
        Index: int
        Name: string
        Type: DeviceType
        IsDefault: bool
    }
    override this.ToString() = this.Name

module Audio =

    /// In BASS units; 10000 is the actual max
    let MAX_VOLUME = 8000.0

    let private fft: float32 array = Array.zeroCreate 1024
    let waveform: float32 array = Array.zeroCreate 256

    let private update_waveform (elapsed_ms: float) : unit =
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

    let update (elapsed_ms: float) : unit =
        update_waveform elapsed_ms
        Song.update elapsed_ms

    let change_volume (fx_volume: float, song_volume: float) =
        Bass.GlobalSampleVolume <- int (fx_volume * MAX_VOLUME) |> max 0
        Bass.GlobalStreamVolume <- int (song_volume * MAX_VOLUME) |> max 0

    let private current_device_changed_ev = Event<unit>()
    let current_device_changed = current_device_changed_ev.Publish

    let NO_AUDIO_DEVICE = 0
    let FIRST_REAL_DEVICE = if OperatingSystem.IsLinux() then 2 else 1

    let mutable private initialised_devices = Set.empty

    let mutable private detected_devices = [||]
    let private detect_devices () =
        detected_devices <-
            seq {
                for i = FIRST_REAL_DEVICE to Bass.DeviceCount - 1 do
                    let ok, info = Bass.GetDeviceInfo i
                    if ok then
                        if info.IsEnabled then
                            yield {
                                Index = i
                                Name = info.Name
                                Type = info.Type
                                IsDefault =
                                    info.IsDefault
                                    || info.Name.Contains("PulseAudio", StringComparison.OrdinalIgnoreCase)
                                    || info.Name.Contains("PipeWire", StringComparison.OrdinalIgnoreCase)
                            }
                    else Logging.Error "Failed to get info for BASS device %i" i
            }
            |> Array.ofSeq

    let list_devices () : Device array = detected_devices

    let change_device (index: int) : unit =
        try
            let device_index =
                match detected_devices |> Array.tryFind (fun x -> x.Index = index) with
                | None ->
                    if index >= 0 then Logging.Debug "No audio device with index %i, using default" index
                    match detected_devices |> Array.tryFind (fun x -> x.IsDefault) with
                    | None ->
                        Logging.Error "No default audio device! Initialising with no sound :("
                        if detected_devices.Length > 0 then
                            Logging.Info "Commonly on macOS and Linux, the engine doesn't know which device is default, but this doesn't mean no devices are available"
                            Logging.Info "There are still %i audio devices detected, switch to one of them in settings" detected_devices.Length
                        NO_AUDIO_DEVICE
                    | Some d -> d.Index
                | Some d -> d.Index

            if initialised_devices.Contains device_index || Bass.Init(device_index, Flags = DeviceInitFlags.Latency) then
                initialised_devices <- initialised_devices.Add device_index
                Bass.CurrentDevice <- device_index
            else
                Logging.Error "Failed to initialise audio device with index %i: %O" device_index Bass.LastError
                if device_index <> NO_AUDIO_DEVICE && (initialised_devices.Contains NO_AUDIO_DEVICE || Bass.Init(NO_AUDIO_DEVICE, Flags = DeviceInitFlags.Latency)) then
                    Bass.CurrentDevice <- NO_AUDIO_DEVICE
                    Logging.Debug "Initialised with no sound :("
                else
                    Logging.Critical "Initialising with no sound also failed: %O" Bass.LastError

            if Song.now_playing.ID <> 0 then
                Bass.ChannelSetDevice(Song.now_playing.ID, device_index) |> display_bass_error

            current_device_changed_ev.Trigger()

        with err ->
            Logging.Error "Unhandled exception switching to audio device %i: %O" index err

    let debug_info() : string =

        let device_dump (dev: Device) : string =
            sprintf "  %i. %s | %O | DEFAULT: %A" dev.Index dev.Name dev.Type dev.IsDefault

        let devices = detected_devices |> Seq.map device_dump |> String.concat "\n"

        match Bass.GetInfo() with
        | false, _ -> sprintf "-- AUDIO DEBUG INFO --\nBass Version: %O\nDetected and enabled devices:\n%s\nOther details could not be retrieved" Bass.Version devices
        | true, info ->
            sprintf
                """-- AUDIO DEBUG INFO --
BASS Version: %O
Estimated latency: %i
Min buffer length: %i
DirectSound Version: %i
Detected and enabled devices:
%s"""
                Bass.Version
                info.Latency
                info.MinBufferLength
                info.DSVersion
                devices

    let init (device: int, device_period: int, device_buffer_length: int) =
        detect_devices ()

        // https://github.com/ppy/osu/issues/3800
        Bass.Configure(Configuration.DevNonStop, true) |> display_bass_error
        Bass.Configure(Configuration.DevicePeriod, device_period) |> display_bass_error
        Bass.Configure(Configuration.DeviceBufferLength, device_buffer_length) |> display_bass_error
        Bass.Configure(Configuration.PlaybackBufferLength, 200) |> display_bass_error
        Bass.Configure(Configuration.UpdatePeriod, 5) |> display_bass_error

        change_device device
        Bass.GlobalStreamVolume <- 0