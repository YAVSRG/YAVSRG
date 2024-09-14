namespace Interlude.Features.Gameplay

open System
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Patterns
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay.Mods
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Prelude.Data
open Interlude.Content
open Interlude.Options
open Interlude.UI

type LoadingChartInfo =
    {
        CacheInfo: CachedChart
        LibraryContext: LibraryContext
    }

type LoadedChartInfo =
    {
        CacheInfo: CachedChart
        LibraryContext: LibraryContext
        DurationString: string
        BpmString: string

        Chart: Chart
        SaveData: ChartSaveData

        WithMods: ModdedChart
        NotecountsString: string
        Rating: DifficultyRating
        Patterns: PatternInfo

        WithColors: ColoredChart
    }

module SelectedChart =

    let mutable autoplay = false

    let _rate = Setting.rate 1.0f
    let _selected_mods = options.SelectedMods

    let private format_duration (cc: CachedChart option) =
        match cc with
        | Some cc -> cc.Length
        | None -> 0.0f<ms>
        |> fun x -> x / _rate.Value
        |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
        |> fun (x, y) -> sprintf "%s %i:%02i" Icons.CLOCK x y

    let private format_bpm (cc: CachedChart option) =
        match cc with
        | Some cc -> cc.BPM
        | None -> (500.0f<ms / beat>, 500.0f<ms / beat>)
        |> fun (b, a) -> (60000.0f<ms> / a * _rate.Value |> int, 60000.0f<ms> / b * _rate.Value |> int)
        |> fun (a, b) ->
            if a > 9000 || b < 0 then
                sprintf "%s ∞" Icons.MUSIC
            elif Math.Abs(a - b) < 5 || b > 9000 then
                sprintf "%s %i" Icons.MUSIC a
            else
                sprintf "%s %i-%i" Icons.MUSIC a b

    let private format_notecounts (chart: ModdedChart) =
        let mutable notes = 0
        let mutable lnotes = 0

        for { Data = nr } in chart.Notes do
            for n in nr do
                if n = NoteType.NORMAL then
                    notes <- notes + 1
                elif n = NoteType.HOLDHEAD then
                    notes <- notes + 1
                    lnotes <- lnotes + 1

        let hold_count =
            let pc = (100.0f * float32 lnotes / float32 notes)

            if pc < 0.5f then
                sprintf "%i Holds" lnotes
            else
                sprintf "%.0f%% Holds" pc

        sprintf "%iK | %i Notes | %s" chart.Keys notes hold_count

    let mutable CACHE_DATA: CachedChart option = None
    let mutable FMT_DURATION: string = format_duration None
    let mutable FMT_BPM: string = format_bpm None
    let mutable LIBRARY_CTX = LibraryContext.None

    let mutable CHART: Chart option = None
    let mutable SAVE_DATA: ChartSaveData option = None

    let mutable WITH_MODS: ModdedChart option = None
    let mutable FMT_NOTECOUNTS: string option = None
    let mutable RATING: DifficultyRating option = None
    let mutable PATTERNS: PatternInfo option = None

    let mutable WITH_COLORS: ColoredChart option = None

    let private create_loaded_chart_info () =
        {
            CacheInfo = CACHE_DATA.Value
            LibraryContext = LIBRARY_CTX
            DurationString = FMT_DURATION
            BpmString = FMT_BPM

            Chart = CHART.Value
            SaveData = SAVE_DATA.Value

            WithMods = WITH_MODS.Value
            NotecountsString = FMT_NOTECOUNTS.Value
            Rating = RATING.Value
            Patterns = PATTERNS.Value

            WithColors = WITH_COLORS.Value
        }


    let private chart_update_finished = Event<LoadedChartInfo>()
    let on_chart_update_finished = chart_update_finished.Publish

    let private chart_change_started = Event<LoadingChartInfo>()
    let on_chart_change_started = chart_change_started.Publish

    let private chart_change_finished = Event<LoadedChartInfo>()
    let on_chart_change_finished = chart_change_finished.Publish

    let private chart_change_failed = Event<unit>()
    let on_chart_change_failed = chart_change_failed.Publish

    let mutable private on_load_succeeded = []

    type private LoadRequest =
        | Load of CachedChart * play_audio: bool * rate: float32 * mods: ModState
        | Update of bool * rate: float32 * mods: ModState
        | Recolor

    let private chart_loader =
        { new Async.SwitchServiceSeq<LoadRequest, unit -> unit>() with
            override this.Process(req) =
                match req with
                | Load(cc, play_audio, rate, mods) ->
                    seq {
                        match Cache.load cc Content.Cache with
                        | Error reason ->
                                
                            Logging.Error(sprintf "Couldn't load chart: %s" reason)
                            Background.load None

                            Notifications.error (
                                %"notification.chart_load_failed.title",
                                %"notification.chart_load_failed.body"
                            )

                            yield
                                fun () -> 
                                    on_load_succeeded <- []
                                    chart_change_failed.Trigger ()
                        | Ok chart ->

                        Background.load (Cache.background_path chart Content.Cache)

                        let save_data = UserDatabase.get_chart_data cc.Hash Content.UserData

                        yield
                            fun () ->
                                CHART <- Some chart

                                Song.change (
                                    Cache.audio_path chart Content.Cache,
                                    save_data.Offset,
                                    rate,
                                    (chart.Header.PreviewTime, chart.LastNote),
                                    (
                                        if play_audio then
                                            if Screen.current_type = Screen.Type.MainMenu then 
                                                SongLoadAction.PlayFromBeginning 
                                            else 
                                                SongLoadAction.PlayFromPreview
                                        else SongLoadAction.Wait
                                    )
                                )

                                SAVE_DATA <- Some save_data
                        // if chart is loaded we can safely restart from this point for different rates and mods

                        let with_mods = Mods.apply mods chart
                        let with_colors = NoteColors.apply (Content.NoteskinConfig.NoteColors) with_mods

                        let rating = DifficultyRating.calculate rate with_mods.Notes

                        let patterns = PatternSummary.generate_pattern_data rate chart
                        let note_counts = format_notecounts with_mods

                        yield
                            fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                PATTERNS <- Some patterns
                                FMT_NOTECOUNTS <- Some note_counts
                                chart_change_finished.Trigger(create_loaded_chart_info ())

                        yield
                            fun () ->
                                for action in on_load_succeeded do
                                    action ()

                                on_load_succeeded <- []
                    }
                | Update(is_interrupted_load, rate, mods) ->
                    seq {
                        match CHART with
                        | None -> ()
                        | Some chart ->

                        let with_mods = Mods.apply mods chart
                        let with_colors = NoteColors.apply (Content.NoteskinConfig.NoteColors) with_mods

                        let rating = DifficultyRating.calculate rate with_mods.Notes

                        let patterns = PatternSummary.generate_pattern_data rate chart
                        let note_counts = format_notecounts with_mods

                        yield
                            fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                PATTERNS <- Some patterns
                                FMT_NOTECOUNTS <- Some note_counts

                                if is_interrupted_load then
                                    chart_change_finished.Trigger(create_loaded_chart_info ())
                                else
                                    chart_update_finished.Trigger(create_loaded_chart_info ())

                        yield
                            fun () ->
                                for action in on_load_succeeded do
                                    action ()

                                on_load_succeeded <- []
                    }
                | Recolor ->
                    seq {
                        match WITH_MODS with
                        | None -> ()
                        | Some with_mods ->

                        let with_colors = NoteColors.apply (Content.NoteskinConfig.NoteColors) with_mods
                        yield fun () -> WITH_COLORS <- Some with_colors

                        yield
                            fun () ->
                                for action in on_load_succeeded do
                                    action ()

                                on_load_succeeded <- []
                    }

            override this.Handle(action) = action ()
        }

    let keymode () : Keymode =
        match CACHE_DATA with
        | Some cc -> cc.Keys |> enum
        | None -> Keymode.``4K``

    let change (cc: CachedChart, ctx: LibraryContext, auto_play_audio: bool) =
        // todo: show a one-time warning if chart loading takes over 1 second
        CACHE_DATA <- Some cc
        LIBRARY_CTX <- ctx
        options.CurrentChart.Value <- cc.Key

        CHART <- None
        SAVE_DATA <- None

        WITH_MODS <- None
        FMT_NOTECOUNTS <- None
        RATING <- None
        PATTERNS <- None

        WITH_COLORS <- None

        chart_change_started.Trigger
            {
                CacheInfo = cc
                LibraryContext = LIBRARY_CTX
            }

        FMT_DURATION <- format_duration CACHE_DATA
        FMT_BPM <- format_bpm CACHE_DATA

        chart_loader.Request(Load(cc, auto_play_audio, _rate.Value, _selected_mods.Value))

    let update () =
        if CACHE_DATA.IsSome then

            FMT_DURATION <- format_duration CACHE_DATA
            FMT_BPM <- format_bpm CACHE_DATA

        if CHART.IsSome then

            let is_interrupted = WITH_MODS.IsNone

            WITH_MODS <- None
            FMT_NOTECOUNTS <- None
            RATING <- None
            PATTERNS <- None

            WITH_COLORS <- None

            chart_loader.Request(Update(is_interrupted, _rate.Value, _selected_mods.Value))

    let recolor () =
        if WITH_MODS.IsSome then

            WITH_COLORS <- None

            chart_loader.Request Recolor

    let if_loading (action: LoadingChartInfo -> unit) =
        if CACHE_DATA.IsSome then
            action
                {
                    CacheInfo = CACHE_DATA.Value
                    LibraryContext = LIBRARY_CTX
                }

    let if_loaded (action: LoadedChartInfo -> unit) =
        if WITH_COLORS.IsSome then
            action (create_loaded_chart_info ())

    let when_loaded (action: LoadedChartInfo -> unit) =
        if WITH_COLORS.IsSome then
            action (create_loaded_chart_info ())
        else
            on_load_succeeded <- (fun () -> action (create_loaded_chart_info ())) :: on_load_succeeded

    let private collections_on_rate_changed (library_ctx: LibraryContext) (v: float32) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Rate.Value <- v
        | _ -> ()

    let private collections_on_mods_changed (library_ctx: LibraryContext) (mods: ModState) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Mods.Value <- mods
        | _ -> ()

    let private collections_on_chart_changed
        (library_ctx: LibraryContext)
        (rate: Setting.Bounded<float32>)
        (mods: Setting<ModState>)
        =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) ->
            rate.Value <- d.Rate.Value
            mods.Value <- d.Mods.Value
        | _ -> ()

    let private presets_on_chart_changed =
        let mutable previous_keymode = None

        fun (cc: CachedChart) ->
            match previous_keymode with
            | Some k when k <> cc.Keys -> Presets.keymode_changed cc.Keys
            | _ -> ()

            previous_keymode <- Some cc.Keys

    let rate =
        _rate
        |> Setting.trigger (fun v ->
            if_loading
            <| fun info ->
                collections_on_rate_changed info.LibraryContext v
                Song.change_rate v
                update ()
        )

    let selected_mods =
        _selected_mods
        |> Setting.trigger (fun mods ->
            if_loading
            <| fun info ->
                collections_on_mods_changed info.LibraryContext mods
                update ()
        )

    let change_rate_hotkeys(change_rate_by: float32 -> unit) =
        if (%%"uprate_small").Tapped() then
            change_rate_by (0.01f)
        elif (%%"uprate_half").Tapped() then
            change_rate_by (0.05f)
        elif (%%"uprate_big").Tapped() then
            change_rate_by (0.25f)
        elif (%%"uprate").Tapped() then
            change_rate_by (0.1f)
        elif (%%"downrate_small").Tapped() then
            change_rate_by (-0.01f)
        elif (%%"downrate_half").Tapped() then
            change_rate_by (-0.05f)
        elif (%%"downrate_big").Tapped() then
            change_rate_by (-0.25f)
        elif (%%"downrate").Tapped() then
            change_rate_by (-0.1f)

    let init_window () = 

        match Cache.by_key options.CurrentChart.Value Content.Cache with
        | Some cc -> change (cc, LibraryContext.None, true)
        | None ->
            Logging.Info("Couldn't find cached chart: " + options.CurrentChart.Value)

            match
                Suggestion.get_random
                    Filter.Empty
                    {
                        Rate = rate.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        UserDatabase = Content.UserData
                    }
            with
            | Some cc -> change (cc, LibraryContext.None, true)
            | None ->
                Logging.Debug "No charts installed (or loading failed twice)"
                Background.load None
        
        on_chart_change_started.Add(fun info -> collections_on_chart_changed info.LibraryContext _rate _selected_mods)
        on_chart_change_started.Add(fun info -> presets_on_chart_changed info.CacheInfo)

        add_to_update_loop chart_loader.Join