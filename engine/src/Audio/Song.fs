namespace Percyqaz.Flux.Audio

open System.Diagnostics
open ManagedBass
open ManagedBass.Fx
open Percyqaz.Common

type Song =
    {
        mutable ID: int
        Frequency: int // in hz
        Duration: Time
        PreviewPoint: Time
        LastNote: Time
        mutable LocalOffset: Time
        mutable LowPassFilterEffect: int
        mutable Playing: bool
    }

    static member CreatePlaceholder(preview_point: Time, last_note: Time, local_offset: Time) : Song =
        {
            ID = 0
            Frequency = 1
            Duration = 1000.0f<ms>
            PreviewPoint = preview_point
            LastNote = last_note
            LocalOffset = local_offset
            LowPassFilterEffect = 0
            Playing = false
        }

    static member CreateFromFile(file: string, preview_point: Time, last_note: Time, local_offset: Time) : Song =
        let handle = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan ||| BassFlags.Decode)

        if handle = 0 then
            let error =
                match Bass.LastError with
                | Errors.FileOpen -> "File couldn't be found/opened OR contains an audio format not supported"
                | other -> other.ToString()

            Logging.Error "Couldn't load audio track from '%s': %s" file error
            Song.CreatePlaceholder(preview_point, last_note, local_offset)
        else
            let duration_seconds =
                Bass.ChannelBytes2Seconds(handle, Bass.ChannelGetLength(handle))

            let duration = float32 duration_seconds * 1000.0f<ms>
            let channel_info = Bass.ChannelGetInfo(handle)
            let frequency = channel_info.Frequency

            let fx_handle = BassFx.TempoCreate(handle, BassFlags.FxFreeSource)
            Bass.ChannelSetDevice(fx_handle, Bass.CurrentDevice).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.NoBuffer, 1f).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoUseQuickAlgorithm, 1).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoOverlapMilliseconds, 4).LogBassError()
            Bass.ChannelSetAttribute(fx_handle, ChannelAttribute.TempoSequenceMilliseconds, 30).LogBassError()

            {
                ID = fx_handle
                Frequency = frequency
                Duration = duration
                PreviewPoint =
                    if preview_point >= duration - 3000.0f<ms> || preview_point < 0.0f<ms> then
                        duration * 0.5f
                    else
                        preview_point
                LastNote = last_note
                LocalOffset = local_offset
                LowPassFilterEffect = 0
                Playing = false
            }

    member this.TimeToBytes(time: Time) : int64 =
        Bass.ChannelSeconds2Bytes(this.ID, time / 1000.0f<ms> |> float)

    member this.CurrentPosition() : Time =
        float32(Bass.ChannelBytes2Seconds(this.ID, Bass.ChannelGetPosition(this.ID))) * 1000.0f<ms>

    member this.ChangeRate(rate: float32<rate>, change_pitch: bool) : unit =

        let set_frequency_multiplier (rate: float32<rate>) =
            let scaled_frequency = float32 this.Frequency * float32 rate
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Frequency, scaled_frequency).LogBassError()

        let set_tempo (rate: float32<rate>) =
            let tempo_adjustment = (rate - 1.0f<rate>) * 100.0f< / rate>
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Tempo, tempo_adjustment).LogBassError()

        if this.ID <> 0 then
            if change_pitch then
                set_tempo(1.0f<rate>)
                set_frequency_multiplier(rate)
            else
                set_tempo(rate)
                set_frequency_multiplier(1.0f<rate>)

    member this.SetLowPassFilter(amount: float32) : unit =

        let apply_effect (handle: int) =
            let center = lerp amount 22049f 800f
            let parameters = BQFParameters(lFilter = BQFType.LowPass, fQ = 0.7f)
            parameters.fCenter <- center
            parameters.fBandwidth <- 0f
            Bass.FXSetParameters(handle, parameters).LogBassError()

        match this.LowPassFilterEffect with
        | _ when this.ID = 0 -> ()
        | 0 ->
            if amount > 0.0f then
                let effect = Bass.ChannelSetFX(this.ID, EffectType.BQF, 1)
                apply_effect(effect)
                this.LowPassFilterEffect <- effect

        | existing_handle when amount = 0.0f ->
            Bass.ChannelRemoveFX(this.ID, existing_handle).LogBassError()
            this.LowPassFilterEffect <- 0

        | existing_handle -> apply_effect(existing_handle)

    member this.Pause() : Time =
        if this.ID <> 0 && this.Playing then
            this.Playing <- false

            if Bass.ChannelIsActive(this.ID) = PlaybackState.Playing then
                Bass.ChannelPause(this.ID).LogBassError()

        this.CurrentPosition()

    member this.Play(delay_callback: float32<ms / rate> -> unit) : unit =
        if this.ID <> 0 && not this.Playing then
            this.Playing <- true
            let before = Stopwatch.GetTimestamp()
            let sync_flags = SyncFlags.Position ||| SyncFlags.Onetime

            let callback =
                SyncProcedure(fun _ _ _ _ ->
                    delay_callback(float32(Stopwatch.GetElapsedTime(before).TotalMilliseconds) * 1.0f<ms / rate>)
                )

            Bass.ChannelSetSync(this.ID, sync_flags, Bass.ChannelGetPosition(this.ID) + 1L, callback).LogBassError()
            Bass.ChannelPlay(this.ID).LogBassError()

    member this.Seek(time: Time) : unit =
        if this.Playing then
            failwithf "Must not seek while song is playing"

        if this.ID <> 0 then
            Bass.ChannelSetPosition(this.ID, this.TimeToBytes(time)).LogBassError()

    member this.Free() : unit =
        if this.ID <> 0 then
            Bass.StreamFree(this.ID).LogBassError()
            this.ID <- 0

    member this.FadeIn(duration_ms: int) : unit =
        if this.ID <> 0 then
            Bass.ChannelSetAttribute(this.ID, ChannelAttribute.Volume, 0.0f).LogBassError()
            Bass.ChannelSlideAttribute(this.ID, ChannelAttribute.Volume, 1.0f, duration_ms).LogBassError()

    member this.FadeOutThenFree(duration_ms: int) : unit =
        if this.ID <> 0 then
            let slide_completed _ _ _ _ = this.Free()
            let callback = SyncProcedure(slide_completed)
            Bass.ChannelSetSync(this.ID, SyncFlags.Slided ||| SyncFlags.Onetime, 0, callback).LogBassError()
            Bass.ChannelSetSync(this.ID, SyncFlags.End ||| SyncFlags.Onetime, 0, callback).LogBassError()
            Bass.ChannelSlideAttribute(this.ID, ChannelAttribute.Volume, 0.0f, duration_ms).LogBassError()
