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
        Duration: float32<ms>
        mutable LowPassFilterEffect: int
    }
    static member Default =
        {
            ID = 0
            Frequency = 1
            Duration = 1000.0f<ms>
            LowPassFilterEffect = 0
        }

    static member FromFile(file: string) =
        let ID = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan ||| BassFlags.Decode)

        if ID = 0 then
            Logging.Error("Couldn't load audio track from " + file, Bass.LastError)
            Song.Default
        else
            let d = Bass.ChannelGetInfo ID
            let Duration = Bass.ChannelBytes2Seconds(ID, Bass.ChannelGetLength ID) * 1000.0
            let Frequency = d.Frequency
            let ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource)
            Bass.ChannelSetDevice(ID, current_device) |> display_bass_error
            Bass.ChannelSetAttribute(ID, ChannelAttribute.NoBuffer, 1f) |> display_bass_error

            {
                ID = ID
                Frequency = Frequency
                Duration = float32 Duration * 1.0f<ms>
                LowPassFilterEffect = 0
            }

    member this.Free() =
        Bass.StreamFree this.ID |> display_bass_error

    member this.SetLowPass(amount: float32) =
        match this.LowPassFilterEffect with
        | 0 when amount > 0.0f ->
            let effect = Bass.ChannelSetFX(this.ID, EffectType.BQF, 1)
            let center = lerp amount 22049f 800f
            Bass.FXSetParameters(effect, BQFParameters(lFilter = BQFType.LowPass, fCenter = center, fBandwidth = 0f, fQ = 0.7f)) |> display_bass_error
            this.LowPassFilterEffect <- effect
        | existing when amount = 0.0f ->
            Bass.ChannelRemoveFX(this.ID, existing) |> display_bass_error
            this.LowPassFilterEffect <- 0
        | existing ->
            let center = lerp amount 22049f 800f
            Bass.FXSetParameters(existing, BQFParameters(lFilter = BQFType.LowPass, fCenter = center, fBandwidth = 0f, fQ = 0.7f)) |> display_bass_error

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

    let private on_loaded_ev = Event<unit>()
    let on_loaded = on_loaded_ev.Publish

    let mutable load_path: string option = None
    let mutable loading = false

    let mutable now_playing: Song = Song.Default
    let private timer = new Stopwatch()
    let mutable private timer_start = 0.0f<ms>
    let mutable private channel_playing = false
    let mutable private paused = false
    let mutable private rate = 1.0f<rate>
    let mutable _local_offset = 0.0f<ms>
    let mutable private _global_offset = 0.0f<ms / rate>
    let mutable on_finish = SongFinishAction.Wait
    let mutable private preview_point = 0.0f<ms>
    let mutable private last_note = 0.0f<ms>
    let mutable private enable_pitch_rates = true

    let mutable private low_pass_amount = 0.0f
    let mutable private low_pass_target = 0.0f

    let duration () = now_playing.Duration

    let time () =
        rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms / rate>) + timer_start

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
        paused <- false

    let play_leadin (first_note: Time) =
        play_from (min 0.0f<ms> (first_note - LEADIN_TIME * rate))

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
        paused <- true

    let resume () =
        let time = time ()

        if time >= 0.0f<ms> && time < duration () then
            Bass.ChannelPlay now_playing.ID |> display_bass_error

        timer.Start()
        paused <- false

    let change_rate (new_rate) =
        let rate_changed = rate <> new_rate
        let time = time ()
        rate <- new_rate

        if not enable_pitch_rates then
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Pitch, -Math.Log(float rate, 2.0) * 12.0)
            |> display_bass_error

        Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency * float32 rate)
        |> display_bass_error

        if rate_changed then
            seek time

    let playback_rate () = rate

    let set_pitch_rates_enabled (enabled: bool) =
        enable_pitch_rates <- enabled

        if now_playing.ID <> 0 then
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Pitch, if enabled then 0.0 else -Math.Log(float rate, 2.0) * 12.0)
            |> display_bass_error

    let set_low_pass (amount: float32) =
        low_pass_target <- amount
        now_playing.SetLowPass low_pass_amount

    let set_local_offset (offset) = _local_offset <- offset
    let set_global_offset (offset) = _global_offset <- offset

    let private song_loader =
        { new Async.SwitchService<string option * SongLoadAction, Song * SongLoadAction>() with
            override this.Process((path, after_load)) =
                async {
                    return
                        match path with
                        | Some p -> Song.FromFile p, after_load
                        | None -> Song.Default, after_load
                }

            override this.Handle((song, after_load: SongLoadAction)) =
                loading <- false
                now_playing <- song
                change_rate rate

                song.SetLowPass low_pass_amount

                match after_load with
                | SongLoadAction.PlayFromPreview ->
                    (if paused then seek else play_from) preview_point
                | SongLoadAction.PlayFromBeginning ->
                    (if paused then seek else play_from) 0.0f<ms>
                | SongLoadAction.Wait -> ()

                on_loaded_ev.Trigger()
        }

    let change (path: string option, offset: Time, new_rate: float32<rate>, (preview: Time, chart_last_note: Time), after_load: SongLoadAction) =
        let path_changed = path <> load_path
        load_path <- path
        preview_point <- preview
        last_note <- chart_last_note
        set_local_offset offset
        change_rate new_rate

        if path_changed then
            timer_start <- -infinityf * 1.0f<ms>

            if now_playing.ID <> 0 then
                now_playing.Free()

            channel_playing <- false
            loading <- true
            song_loader.Request(path, after_load)

    let update (elapsed_ms: float) =

        if low_pass_target <> low_pass_amount then
            low_pass_amount <- lerp (float32 <| Math.Pow(0.994, elapsed_ms)) low_pass_target low_pass_amount
            if abs (low_pass_amount - low_pass_target) < 0.01f then low_pass_amount <- low_pass_target
            now_playing.SetLowPass low_pass_amount

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