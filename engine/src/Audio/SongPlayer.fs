namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open Percyqaz.Common

[<RequireQualifiedAccess>]
type SongFinishAction =
    | LoopFromPreview
    | LoopFromBeginning
    | Wait
    | Custom of (unit -> unit)

[<RequireQualifiedAccess>]
[<Struct>]
type SongLoadAction =
    | PlayFromPreview
    | PlayFromBeginning
    | Wait

[<Struct>]
type SongLoadRequest =
    {
        Path: string option
        AfterLoad: SongLoadAction
        PreviewPoint: Time
        LastNote: Time
        LocalOffset: Time
    }

[<Struct>]
type private CurrentSongState =
    // Audio time is not moving
    | Paused
    // Time is moving with no audio, either:
    // - Time is negative, to give leadin time to audio, and when it passes 0 we will start playing audio
    // - OR There is no song audio loaded so we are playing through the expected duration without audio
    // - OR We have requested that BASS play audio but there is a ~37ms delay (it varies)
    //   between it receiving the request and the audio actually buffering and reaching audio drivers
    //   Once BASS reports that audio is playing, state changes to PlayingWithAudio
    | PlayingWithoutAudio
    // Time is moving, The song is playing, BASS has reported that audio is making it to drivers
    | PlayingWithAudio

type SongPlayer() =

    let mutable song: Song = Song.CreatePlaceholder(0.0f<ms>, 1000.0f<ms>, 0.0f<ms>)

    let on_loaded_ev = Event<unit>()

    let mutable load_path: string option = None
    let mutable loading = false
    let mutable on_finish = SongFinishAction.Wait

    let mutable timer_start: int64 = Stopwatch.GetTimestamp()
    let mutable timer_position: Time = 0.0f<ms>
    let mutable state: CurrentSongState = PlayingWithoutAudio

    [<Literal>]
    let ASSUMED_BASS_DELAY = 37.0f<ms / rate>

    let mutable actual_bass_delay = 37.0f<ms / rate>
    let mutable time_at_start_of_frame = 0.0f<ms>
    let mutable compensation_at_start_of_frame = ASSUMED_BASS_DELAY

    let delay_callback =
        fun delay ->
            actual_bass_delay <- delay
            state <- PlayingWithAudio

    let mutable rate = 1.0f<rate>
    let mutable enable_pitch_rate_down = true
    let mutable enable_pitch_rate_up = true
    let mutable global_offset = 0.0f<ms / rate>
    let mutable crossfade_enabled = true

    let mutable low_pass_amount = 0.0f
    let mutable low_pass_target = 0.0f

    static member val LEADIN_TIME = 3000.0f<ms / rate>
    static member val CROSSFADE_DURATION_MS = 200

    member this.IsLoading: bool = loading
    member this.OnLoaded: IEvent<unit> = on_loaded_ev.Publish

    member this.Current: Song = song

    /// The exact audio position the engine thinks we are at; Can be called mid-frame for sub-frame precision
    member private this.PositionRealtime() : Time =
        if state = Paused then
            timer_position
        else
            timer_position + float32(Stopwatch.GetElapsedTime(timer_start).TotalMilliseconds) * 1.0f<ms / rate> * rate

    /// Used by the input engine at the instant an input is polled for precise mapping of inputs to audio positions
    member this.PositionInputEngine() : Time =
        this.PositionRealtime() - compensation_at_start_of_frame * rate + song.LocalOffset + global_offset * rate

    member private this.MarkStartOfFrame() : unit =
        match state with
        | PlayingWithAudio ->
            let target_compensation = actual_bass_delay - ASSUMED_BASS_DELAY

            let run_time =
                float32(Stopwatch.GetElapsedTime(timer_start).TotalMilliseconds) * 1.0f<ms / rate>

            let multiplier =
                min 1.0f ((run_time - actual_bass_delay) / (1000.0f * abs target_compensation))

            compensation_at_start_of_frame <- ASSUMED_BASS_DELAY + multiplier * target_compensation
        | PlayingWithoutAudio ->
            let run_time =
                float32(Stopwatch.GetElapsedTime(timer_start).TotalMilliseconds) * 1.0f<ms / rate>

            compensation_at_start_of_frame <- min run_time ASSUMED_BASS_DELAY
        | Paused -> compensation_at_start_of_frame <- 0.0f<ms / rate>

        time_at_start_of_frame <- this.PositionRealtime()

    member this.StartOfTimeline(first_note: Time) : Time =
        min 0.0f<ms> (first_note - SongPlayer.LEADIN_TIME * rate)

    member this.IsPlaying: bool = state <> Paused

    member this.PlayFrom(time: Time) : unit =
        if song.ShouldPlayAt(time) then
            song.PlayFrom(time, delay_callback)

        else if song.Playing then
            song.Pause() |> ignore

        timer_position <- time
        timer_start <- Stopwatch.GetTimestamp()
        this.MarkStartOfFrame()
        state <- PlayingWithoutAudio

    member this.PlayLeadin(first_note: Time) : unit =
        this.PlayFrom(this.StartOfTimeline(first_note))

    member this.Seek(time: Time) : unit =
        if this.IsPlaying then
            this.PlayFrom(time)
        else
            if song.ShouldPlayAt(time) then
                song.Seek(time)

            timer_position <- time
            this.MarkStartOfFrame()

    member this.Pause() : unit =
        let time = this.PositionRealtime()
        timer_position <- time

        if song.ShouldPlayAt(time) then
            timer_position <- song.Pause()

        state <- Paused
        this.MarkStartOfFrame()

    member this.Resume() : unit =
        let time = this.PositionRealtime()

        if song.ShouldPlayAt(time) then
            song.PlayFrom(time, delay_callback)

        timer_position <- time
        timer_start <- Stopwatch.GetTimestamp()
        this.MarkStartOfFrame()
        state <- PlayingWithoutAudio

    // Local offset is a Time with `ms` units AKA *chart units* which scale with rate - for offsetting notes relative to audio where they are misaligned
    member this.LocalOffset: Time = song.LocalOffset
    member this.SetLocalOffset(offset: Time) : unit = song.LocalOffset <- offset

    // Global offset is with `ms / rate` units AKA *real time ms*, for offsetting a constant realtime delay with hardware
    member this.GlobalOffset: float32<ms / rate> = global_offset
    member this.SetGlobalOffset(offset: float32<ms / rate>) : unit = global_offset <- offset

    member this.OnFinish(action: SongFinishAction) : unit = on_finish <- action

    member this.RateChangesPitch(rate: float32<rate>) : bool =
        if rate = 1.0f<rate> then true
        elif rate > 1.0f<rate> then enable_pitch_rate_up
        else enable_pitch_rate_down

    member private this.ApplyRateChange() : unit =
        let time = song.Pause()
        song.ChangeRate(rate, this.RateChangesPitch(rate))
        this.Seek(time)

    member this.Rate: float32<rate> = rate

    member this.SetRate(new_rate: float32<rate>) : unit =
        if rate <> new_rate then
            rate <- new_rate
            this.ApplyRateChange()

    member this.DownRatesChangePitch
        with get () = enable_pitch_rate_down
        and set (enabled: bool) =
            enable_pitch_rate_down <- enabled

            if rate < 1.0f<rate> then
                this.ApplyRateChange()

    member this.UpRatesChangePitch
        with get () = enable_pitch_rate_up
        and set (enabled: bool) =
            enable_pitch_rate_up <- enabled

            if rate > 1.0f<rate> then
                this.ApplyRateChange()

    member this.CrossfadeEnabled
        with get () = crossfade_enabled
        and set (enabled: bool) = crossfade_enabled <- enabled

    member this.ApplyLowPassFilter(amount: float32) : unit =
        low_pass_target <- amount
        song.SetLowPassFilter(low_pass_amount)

    member this.ApplyLowPassFilter() : unit = this.ApplyLowPassFilter(1.0f)

    member this.RemoveLowPassFilter() : unit = this.ApplyLowPassFilter(0.0f)

    member private this.FinishLoadingSong(new_song: Song, after_load: SongLoadAction) : unit =
        loading <- false
        new_song.ChangeRate(rate, this.RateChangesPitch(rate))
        new_song.SetLowPassFilter(low_pass_amount)
        new_song.ChangeToCurrentDevice()
        let was_paused = state = Paused
        state <- Paused
        song <- new_song

        match after_load with
        | SongLoadAction.PlayFromPreview ->
            if crossfade_enabled then
                song.FadeIn(SongPlayer.CROSSFADE_DURATION_MS)

            if was_paused then this.Seek(song.PreviewPoint) else this.PlayFrom(song.PreviewPoint)

        | SongLoadAction.PlayFromBeginning -> if was_paused then this.Seek(0.0f<ms>) else this.PlayFrom(0.0f<ms>)

        | SongLoadAction.Wait -> this.Seek(0.0f<ms>)

        on_loaded_ev.Trigger()

    member val AsyncSongLoader =
        { new Async.CancelQueue<SongLoadRequest * SongPlayer, Song * SongLoadAction * SongPlayer>() with
            override this.Process((request: SongLoadRequest, caller: SongPlayer)) : Async<_> =
                async {
                    return
                        match request.Path with
                        | Some file_path ->
                            Song.CreateFromFile(file_path, request.PreviewPoint, request.LastNote, request.LocalOffset)
                        | None -> Song.CreatePlaceholder(request.PreviewPoint, request.LastNote, request.LocalOffset)
                        , request.AfterLoad
                        , caller
                }

            override this.Handle((new_song: Song, after_load: SongLoadAction, caller: SongPlayer)) : unit =
                caller.FinishLoadingSong(new_song, after_load)
        }

    member this.LoadSong(request: SongLoadRequest, new_rate: float32<rate>) : unit =
        let path_changed = request.Path <> load_path
        rate <- new_rate

        if path_changed then
            load_path <- request.Path
            if crossfade_enabled then song.FadeOutThenFree(SongPlayer.CROSSFADE_DURATION_MS) else song.Free()
            loading <- true
            this.AsyncSongLoader.Request(request, this)
        else
            song.LocalOffset <- request.LocalOffset

    member this.Update(elapsed_ms: float) : unit =

        let update_low_pass_filter_slide (elapsed_ms: float) : unit =
            let is_sliding = low_pass_target <> low_pass_amount

            if is_sliding then
                low_pass_amount <- lerp (float32(Math.Pow(0.994, elapsed_ms))) low_pass_target low_pass_amount
                song.SetLowPassFilter(low_pass_amount)

                let is_slide_complete = abs(low_pass_amount - low_pass_target) < 0.01f

                if is_slide_complete then
                    low_pass_amount <- low_pass_target

        let on_song_finished () : unit =
            match on_finish with
            | SongFinishAction.LoopFromPreview ->
                if time_at_start_of_frame >= song.LastNote then
                    this.PlayFrom(song.PreviewPoint)
            | SongFinishAction.LoopFromBeginning ->
                if time_at_start_of_frame >= song.LastNote then
                    this.PlayFrom(0.0f<ms>)
            | SongFinishAction.Wait -> song.Pause() |> ignore
            | SongFinishAction.Custom action ->
                if song.Playing then
                    action()

        this.MarkStartOfFrame() // Update time_at_start_of_frame, compensation_at_start_of_frame
        update_low_pass_filter_slide(elapsed_ms)
        this.AsyncSongLoader.Join()

        if state.IsPlayingWithoutAudio && not song.Playing && song.ShouldPlayAt(time_at_start_of_frame) then
            song.PlayFrom(time_at_start_of_frame, delay_callback)

        elif time_at_start_of_frame > song.Duration then
            on_song_finished()

    // todo: go through and property identify/label these
    member this.FrameCompensation = this.PositionRealtime() - time_at_start_of_frame
    member this.Time = time_at_start_of_frame
    member this.TimeCompensated = this.Time - compensation_at_start_of_frame * rate

    member this.TimeWithOffset =
        this.TimeCompensated + song.LocalOffset + global_offset * rate

    member internal this.DebugCompensationAtStartOfFrame = compensation_at_start_of_frame
    member internal this.DebugActualBassDelay = actual_bass_delay

    member internal this.DebugTimelineDrift() : Time =
        if song.ID = 0 then
            0.0f<ms>
        else
            let bass_reported_time = song.CurrentPosition()
            let timeline_time = this.PositionRealtime() - compensation_at_start_of_frame * rate
            timeline_time - bass_reported_time
