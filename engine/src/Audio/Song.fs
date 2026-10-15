namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open ManagedBass
open ManagedBass.Fx
open Percyqaz.Common

type Song =
    {
        ID: int
        Frequency: int // in hz
        Duration: Time
        PreviewPoint: Time
        LastNote: Time
        mutable LocalOffset: Time
        mutable LowPassFilterEffect: int
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
            }

    member this.TimeToBytes(time: Time) : int64 =
        Bass.ChannelSeconds2Bytes(this.ID, time / 1000.0f<ms> |> float)

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

    member this.Free() : unit =
        if this.ID <> 0 then
            Bass.StreamFree(this.ID).LogBassError()

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

    let LEADIN_TIME = 3000.0f<ms / rate>
    let CROSSFADE_DURATION_MS = 200

    let private on_loaded_ev = Event<unit>()
    let on_loaded = on_loaded_ev.Publish

    let mutable private load_path: string option = None
    let mutable loading = false

    let mutable internal now_playing: Song =
        Song.CreatePlaceholder(0.0f<ms>, 1000.0f<ms>, 0.0f<ms>)

    let mutable private _time = 0.0f<ms>
    let private timer = Stopwatch()
    let mutable private timer_start = 0.0f<ms>
    let mutable private channel_playing = false
    let mutable private paused = false
    let mutable on_finish = SongFinishAction.Wait
    let mutable internal seek_inaccuracy_compensation = 37.0f<ms>

    module AudioSettings =

        let mutable rate = 1.0f<rate>
        let mutable enable_pitch_rate_down = true
        let mutable enable_pitch_rate_up = true
        let mutable _global_offset = 0.0f<ms / rate>

        let mutable low_pass_amount = 0.0f
        let mutable low_pass_target = 0.0f

    let duration () : Time = now_playing.Duration

    /// The exact audio position the engine thinks we are at; Can be called mid-frame for sub-frame precision
    let private exact_time () : Time =
        timer_start + AudioSettings.rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms / rate>)

    /// Used by the input engine at the instant an input is polled for precise mapping of inputs to audio positions
    let exact_time_with_offset () : Time =
        exact_time() - seek_inaccuracy_compensation
        + now_playing.LocalOffset
        + AudioSettings._global_offset * AudioSettings.rate

    /// For the rest of gameplay rendering, the current 'exact time' is set at the start of the frame
    /// This value is then reused without being updated until next frame
    let private update_time () : unit = _time <- exact_time()

    let frame_compensation () : Time = exact_time() - _time

    let time () : Time = _time

    let time_compensated () : Time = time() - seek_inaccuracy_compensation

    let time_with_offset () : Time =
        time_compensated() + now_playing.LocalOffset + AudioSettings._global_offset * AudioSettings.rate

    let playing () : bool = timer.IsRunning

    let play_from (time: Time) : unit =

        if time >= 0.0f<ms> && time < duration() && now_playing.ID <> 0 then

            let pos = now_playing.TimeToBytes(time)
            Bass.ChannelSetPosition(now_playing.ID, pos).LogBassError()

            Bass
                .ChannelSetSync(
                    now_playing.ID,
                    SyncFlags.Position ||| SyncFlags.Onetime,
                    pos,
                    SyncProcedure(fun _ _ _ _ -> printfn "%.4fms to play" timer.Elapsed.TotalMilliseconds)
                )
                .LogBassError()

            Bass.ChannelPlay(now_playing.ID).LogBassError()
            channel_playing <- true

        else if channel_playing then
            Bass.ChannelStop(now_playing.ID).LogBassError()
            channel_playing <- false

        timer_start <- time
        timer.Restart()
        paused <- false
        update_time()

    let play_leadin (first_note: Time) : unit =
        play_from(min 0.0f<ms> (first_note - LEADIN_TIME * AudioSettings.rate))

    let seek (time: Time) : unit =
        if playing() then
            play_from time
        else
            if time >= 0.0f<ms> && time < duration() && now_playing.ID <> 0 then
                Bass.ChannelSetPosition(now_playing.ID, now_playing.TimeToBytes(time)).LogBassError()
            else if channel_playing then
                Bass.ChannelStop(now_playing.ID).LogBassError()
                channel_playing <- false

            timer_start <- time
            timer.Reset()
            update_time()

    let pause () : unit =
        let time = time()

        if time >= 0.0f<ms> && time < duration() && now_playing.ID <> 0 then
            Bass.ChannelPause(now_playing.ID).LogBassError()

        timer.Stop()
        paused <- true

    let resume () : unit =
        let time = time()

        if time >= 0.0f<ms> && time < duration() && now_playing.ID <> 0 then

            let pos = now_playing.TimeToBytes(time)
            Bass.ChannelSetPosition(now_playing.ID, pos).LogBassError()
            let before = timer.Elapsed.TotalMilliseconds

            Bass
                .ChannelSetSync(
                    now_playing.ID,
                    SyncFlags.Position ||| SyncFlags.Onetime,
                    pos,
                    SyncProcedure(fun _ _ _ _ -> printfn "%.4fms to resume" (timer.Elapsed.TotalMilliseconds - before))
                )
                .LogBassError()

            Bass.ChannelPlay(now_playing.ID).LogBassError()

        timer.Start()
        paused <- false

    type AudioSettings =
        static member LocalOffset
            with get (): Time = now_playing.LocalOffset
            and set (offset: Time) = now_playing.LocalOffset <- offset

        static member GlobalOffset
            with get (): float32<ms / rate> = AudioSettings._global_offset
            and set (offset: float32<ms / rate>) = AudioSettings._global_offset <- offset

        static let handle_rate_change () =
            now_playing.ChangeRate(AudioSettings.rate, AudioSettings.RateChangesPitch())
            seek(time())

        static member RateChangesPitch() : bool =
            if AudioSettings.rate = 1.0f<rate> then true
            elif AudioSettings.rate > 1.0f<rate> then AudioSettings.enable_pitch_rate_up
            else AudioSettings.enable_pitch_rate_down

        static member Rate
            with get (): float32<rate> = AudioSettings.rate
            and set (new_rate: float32<rate>) =
                if AudioSettings.rate <> new_rate then
                    AudioSettings.rate <- new_rate
                    handle_rate_change()

        static member DownRatesChangePitch
            with get () = AudioSettings.enable_pitch_rate_down
            and set (enabled: bool) =
                AudioSettings.enable_pitch_rate_down <- enabled

                if AudioSettings.rate < 1.0f<rate> then
                    handle_rate_change()

        static member UpRatesChangePitch
            with get () = AudioSettings.enable_pitch_rate_up
            and set (enabled: bool) =
                AudioSettings.enable_pitch_rate_up <- enabled

                if AudioSettings.rate > 1.0f<rate> then
                    handle_rate_change()

        static member LowPassFilter
            with set (amount: float32) =
                AudioSettings.low_pass_target <- amount
                now_playing.SetLowPassFilter(AudioSettings.low_pass_amount)

    [<Struct>]
    type SongLoadRequest =
        {
            Path: string option
            AfterLoad: SongLoadAction
            PreviewPoint: Time
            LastNote: Time
            LocalOffset: Time
        }

    let private song_loader =
        { new Async.CancelQueue<SongLoadRequest, Song * SongLoadAction>() with
            override this.Process(request) =
                async {
                    return
                        match request.Path with
                        | Some file_path ->
                            Song.CreateFromFile(file_path, request.PreviewPoint, request.LastNote, request.LocalOffset),
                            request.AfterLoad
                        | None ->
                            Song.CreatePlaceholder(request.PreviewPoint, request.LastNote, request.LocalOffset),
                            request.AfterLoad
                }

            override this.Handle((song, after_load: SongLoadAction)) =
                loading <- false
                song.ChangeRate(AudioSettings.rate, AudioSettings.RateChangesPitch())
                song.SetLowPassFilter(AudioSettings.low_pass_amount)
                now_playing <- song

                match after_load with
                | SongLoadAction.PlayFromPreview ->
                    (if paused then seek else play_from) song.PreviewPoint
                    song.FadeIn(CROSSFADE_DURATION_MS)
                | SongLoadAction.PlayFromBeginning -> (if paused then seek else play_from) 0.0f<ms>
                | SongLoadAction.Wait ->
                    play_from(LEADIN_TIME * AudioSettings.rate)
                    pause()

                on_loaded_ev.Trigger()
        }

    let change (request: SongLoadRequest, new_rate: float32<rate>) : unit =
        let path_changed = request.Path <> load_path
        AudioSettings.Rate <- new_rate

        if path_changed then
            load_path <- request.Path
            now_playing.FadeOutThenFree(CROSSFADE_DURATION_MS)
            channel_playing <- false
            loading <- true
            song_loader.Request(request)
        else
            now_playing.LocalOffset <- request.LocalOffset

    let update (elapsed_ms: float) : unit =
        let time_last_update = _time
        update_time()

        if AudioSettings.low_pass_target <> AudioSettings.low_pass_amount then
            AudioSettings.low_pass_amount <-
                lerp (float32(Math.Pow(0.994, elapsed_ms))) AudioSettings.low_pass_target AudioSettings.low_pass_amount

            if abs(AudioSettings.low_pass_amount - AudioSettings.low_pass_target) < 0.01f then
                AudioSettings.low_pass_amount <- AudioSettings.low_pass_target

            now_playing.SetLowPassFilter(AudioSettings.low_pass_amount)

        song_loader.Join()

        if
            playing() && _time >= 0.0f<ms> && _time < now_playing.Duration && not channel_playing && now_playing.ID <> 0
        then

            channel_playing <- true

            let pos = now_playing.TimeToBytes(_time)
            Bass.ChannelSetPosition(now_playing.ID, pos).LogBassError()

            Bass
                .ChannelSetSync(
                    now_playing.ID,
                    SyncFlags.Position ||| SyncFlags.Onetime,
                    pos,
                    SyncProcedure(fun _ _ _ _ -> printfn "%.4fms to play after leadin" timer.Elapsed.TotalMilliseconds)
                )
                .LogBassError()

            Bass.ChannelPlay(now_playing.ID).LogBassError()

        elif _time > now_playing.Duration then

            match on_finish with
            | SongFinishAction.LoopFromPreview ->
                if _time >= now_playing.LastNote then
                    play_from now_playing.PreviewPoint
            | SongFinishAction.LoopFromBeginning ->
                if _time >= now_playing.LastNote then
                    play_from 0.0f<ms>
            | SongFinishAction.Wait -> ()
            | SongFinishAction.Custom action ->
                if channel_playing then
                    action()

            channel_playing <- false

        elif channel_playing && playing() && timer.ElapsedMilliseconds > 80L && timer.ElapsedMilliseconds < 2000L then
            let audio_position =
                Bass.ChannelBytes2Seconds(now_playing.ID, Bass.ChannelGetPosition(now_playing.ID, PositionFlags.Bytes))
                |> float32

            let clock_elapsed = _time - time_last_update

            let off_by = time_compensated() - audio_position * 1000.0f<ms>
            let adjustment = abs off_by |> min(clock_elapsed * 0.8f)

            if adjustment > 0.5f<ms> && abs off_by < 200.0f<ms> then
                seek_inaccuracy_compensation <-
                    seek_inaccuracy_compensation + if off_by > 0.0f<ms> then adjustment else -adjustment
