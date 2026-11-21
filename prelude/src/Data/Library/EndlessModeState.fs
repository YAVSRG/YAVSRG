namespace Prelude.Data.Library

open System
open Prelude
open Prelude.Data.UserDatabase
open Prelude.Mods
open Prelude.Data.Library

type EndlessModeState =
    internal { mutable Playlist: string; mutable Queue: (ChartMeta * (int * PlaylistEntryInfo)) list }

module EndlessModeState =

    let create () = { Playlist = ""; Queue = [] }

    let private shuffle_playlist_charts (items: 'T seq) =
        let random = Random()
        items |> Seq.map(fun x -> x, random.Next()) |> Seq.sortBy snd |> Seq.map fst

    let queue_playlist
        (from: int)
        (name: string)
        (playlist: Playlist)
        (library: Library)
        (filter: FilteredSearch)
        (state: EndlessModeState)
        =
        state.Playlist <- name

        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.skip from
            |> Seq.choose(fun (i, (c, info)) ->
                match library.Charts.GetChartMeta(c.Hash) with
                | Some chart_meta -> Some(chart_meta, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> List.ofSeq

    let queue_shuffled_playlist
        (name: string)
        (playlist: Playlist)
        (library: Library)
        (filter: FilteredSearch)
        (state: EndlessModeState)
        =
        state.Playlist <- name

        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.choose(fun (i, (c, info)) ->
                match library.Charts.GetChartMeta(c.Hash) with
                | Some chart_meta -> Some(chart_meta, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> shuffle_playlist_charts
            |> List.ofSeq

    let clear_queue (state: EndlessModeState) =
        state.Playlist <- ""
        state.Queue <- []

    type Next =
        {
            ChartMeta: ChartMeta
            Rate: Rate
            Mods: ModState
            LibraryContext: LibraryContext
            NextContext: SuggestionContext
        }

    let next (ctx: SuggestionContext) (state: EndlessModeState) : Next option =
        match state.Queue with
        | (chart_meta, (index, playlist_data)) :: xs ->
            state.Queue <- xs

            Some
                {
                    ChartMeta = chart_meta
                    Rate = playlist_data.Rate.Value
                    Mods = playlist_data.Mods.Value
                    LibraryContext = LibraryContext.Playlist(index, state.Playlist, playlist_data)
                    NextContext = ctx
                }
        | [] ->
            match Suggestion.get_suggestion ctx with
            | Some(next_chart_meta, rate) ->
                Some
                    {
                        ChartMeta = next_chart_meta
                        Rate = rate
                        Mods = ctx.Mods
                        LibraryContext = LibraryContext.None
                        NextContext = ctx
                    }
            | None -> None
