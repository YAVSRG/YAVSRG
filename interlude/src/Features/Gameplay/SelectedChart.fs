namespace Interlude.Features.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay.Mods
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.Content
open Interlude.Options
open Interlude.UI

type LoadingChartInfo =
    {
        CacheInfo: ChartMeta
        LibraryContext: LibraryContext
    }

type LoadedChartInfo =
    {
        CacheInfo: ChartMeta
        LibraryContext: LibraryContext
        DurationString: string
        BpmString: string

        Chart: Chart
        SaveData: ChartSaveData

        WithMods: ModdedChart
        NotecountsString: string
        Rating: DifficultyRating

        WithColors: ColoredChart
    }

module SelectedChart =

    let mutable autoplay = false

    let _rate = Setting.rate 1.0f<rate>
    let _selected_mods = options.SelectedMods

    let private format_duration (cc: ChartMeta option) =
        match cc with
        | Some cc -> cc.Length
        | None -> 0.0f<ms>
        |> fun x -> x / _rate.Value
        |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
        |> fun (x, y) -> sprintf "%s %i:%02i" Icons.CLOCK x y

    let private format_bpm (cc: ChartMeta option) =
        match cc with
        | Some cc -> sprintf "%s %i" Icons.MUSIC (int(float32 cc.BPM * _rate.Value))
        | None -> sprintf "%s 120" Icons.MUSIC

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

    let mutable CACHE_DATA: ChartMeta option = None
    let mutable FMT_DURATION: string = format_duration None
    let mutable FMT_BPM: string = format_bpm None
    let mutable LIBRARY_CTX = LibraryContext.None

    let mutable CHART: Chart option = None
    let mutable SAVE_DATA: ChartSaveData option = None

    let mutable WITH_MODS: ModdedChart option = None
    let mutable FMT_NOTECOUNTS: string option = None
    let mutable RATING: DifficultyRating option = None

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

    let mutable private on_load_succeeded: (bool * (unit -> unit)) list = []
    let mutable private on_song_load_succeeded: (unit -> unit) list = []

    type private LoadRequest =
        | Load of ChartMeta * play_audio: bool * rate: Rate * mods: ModState
        | Update of bool * rate: Rate * mods: ModState
        | Recolor

    let private chart_loader =
        { new Async.SwitchServiceSeq<LoadRequest, unit -> unit>() with
            override this.Process(req) =
                match req with
                | Load(cc, play_audio, rate, mods) ->
                    seq {
                        match ChartDatabase.get_chart cc.Hash Content.Charts with
                        | Error reason ->

                            Logging.Error "Couldn't load chart: %s" reason
                            Background.load None

                            Notifications.error (
                                %"notification.chart_load_failed.title",
                                %"notification.chart_load_failed.body"
                            )

                            yield
                                fun () ->
                                    on_load_succeeded <- []
                                    on_song_load_succeeded <- []
                                    chart_change_failed.Trigger ()
                        | Ok chart ->

                        Background.load cc.Background.Path
                        let save_data = UserDatabase.get_chart_data cc.Hash Content.UserData

                        yield
                            fun () ->
                                CHART <- Some chart

                                Song.change (
                                    cc.Audio.Path,
                                    save_data.Offset,
                                    rate,
                                    (cc.PreviewTime, chart.LastNote),
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

                        let note_counts = format_notecounts with_mods

                        yield
                            fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                FMT_NOTECOUNTS <- Some note_counts
                                chart_change_finished.Trigger(create_loaded_chart_info ())

                                if not Song.loading then
                                    for _, action in on_load_succeeded do
                                        action ()
                                else
                                    on_song_load_succeeded <-
                                        on_load_succeeded |>
                                        List.choose (function | true, action -> Some action | false, action -> action(); None)

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

                        let note_counts = format_notecounts with_mods

                        yield
                            fun () ->
                                WITH_MODS <- Some with_mods
                                WITH_COLORS <- Some with_colors
                                RATING <- Some rating
                                FMT_NOTECOUNTS <- Some note_counts

                                if is_interrupted_load then
                                    chart_change_finished.Trigger(create_loaded_chart_info ())
                                else
                                    chart_update_finished.Trigger(create_loaded_chart_info ())

                                if not Song.loading then
                                    for _, action in on_load_succeeded do
                                        action ()
                                else
                                    on_song_load_succeeded <-
                                        on_load_succeeded |>
                                        List.choose (function | true, action -> Some action | false, action -> action(); None)

                                on_load_succeeded <- []
                    }
                | Recolor ->
                    seq {
                        match WITH_MODS with
                        | None -> ()
                        | Some with_mods ->

                        let with_colors = NoteColors.apply (Content.NoteskinConfig.NoteColors) with_mods
                        yield fun () ->
                            WITH_COLORS <- Some with_colors

                            if not Song.loading then
                                for _, action in on_load_succeeded do
                                    action ()
                            else
                                on_song_load_succeeded <-
                                    on_load_succeeded |>
                                    List.choose (function | true, action -> Some action | false, action -> action(); None)

                            on_load_succeeded <- []
                    }

            override this.Handle(action) = action ()
        }

    let keymode () : Keymode =
        match CACHE_DATA with
        | Some cc -> cc.Keys |> enum
        | None -> Keymode.``4K``

    let change (cc: ChartMeta, ctx: LibraryContext, auto_play_audio: bool) =
        // todo: show a one-time warning if chart loading takes over 1 second
        CACHE_DATA <- Some cc
        LIBRARY_CTX <- ctx
        options.CurrentChart.Value <- cc.Hash

        CHART <- None
        SAVE_DATA <- None

        WITH_MODS <- None
        FMT_NOTECOUNTS <- None
        RATING <- None

        WITH_COLORS <- None

        chart_change_started.Trigger
            {
                CacheInfo = cc
                LibraryContext = LIBRARY_CTX
            }

        FMT_DURATION <- format_duration CACHE_DATA
        FMT_BPM <- format_bpm CACHE_DATA
        on_song_load_succeeded <- []

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

    let when_loaded (also_require_song: bool) (action: LoadedChartInfo -> unit) =
        if WITH_COLORS.IsSome then
            action (create_loaded_chart_info ())
        else
            on_load_succeeded <- (also_require_song, fun () -> action (create_loaded_chart_info ())) :: on_load_succeeded

    let private collections_on_rate_changed (library_ctx: LibraryContext) (v: Rate) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Rate.Value <- v
        | _ -> ()

    let private collections_on_mods_changed (library_ctx: LibraryContext) (mods: ModState) =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) -> d.Mods.Value <- mods
        | _ -> ()

    let private collections_on_chart_changed
        (library_ctx: LibraryContext)
        (rate: Setting.Bounded<Rate>)
        (mods: Setting<ModState>)
        =
        match library_ctx with
        | LibraryContext.Playlist(_, _, d) ->
            rate.Value <- d.Rate.Value
            mods.Value <- d.Mods.Value
        | _ -> ()

    let private presets_on_chart_changed =
        let mutable previous_keymode = None

        fun (cc: ChartMeta) ->
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

    let change_rate_hotkeys(change_rate_by: Rate -> unit) =
        if (%%"uprate_small").Tapped() then
            change_rate_by (0.01f<rate>)
        elif (%%"uprate_half").Tapped() then
            change_rate_by (0.05f<rate>)
        elif (%%"uprate_big").Tapped() then
            change_rate_by (0.25f<rate>)
        elif (%%"uprate").Tapped() then
            change_rate_by (0.1f<rate>)
        elif (%%"downrate_small").Tapped() then
            change_rate_by (-0.01f<rate>)
        elif (%%"downrate_half").Tapped() then
            change_rate_by (-0.05f<rate>)
        elif (%%"downrate_big").Tapped() then
            change_rate_by (-0.25f<rate>)
        elif (%%"downrate").Tapped() then
            change_rate_by (-0.1f<rate>)

    let init_window () =

        match ChartDatabase.get_meta options.CurrentChart.Value Content.Charts with
        | Some cc -> change (cc, LibraryContext.None, true)
        | None ->
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

        Song.on_loaded.Add(fun () ->
            for action in on_song_load_succeeded do action()
            on_song_load_succeeded <- []
        )

        Screen.animations.Add(Animation.ActionLoop(chart_loader.Join)) // todo: tidy up