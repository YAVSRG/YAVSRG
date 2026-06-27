namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Prelude
open Prelude.Data.Library
open Interlude.UI
open Interlude.Features.Gameplay

module LevelSelectHistory =

    let mutable private history: (ChartMeta * Rate) list = []
    let mutable private forward_history: (ChartMeta * Rate) list = []

    let append_current () =
        match SelectedChart.CACHE_DATA with
        | Some chart_meta ->
            forward_history <- []
            history <- (chart_meta, SelectedChart.rate.Value) :: history
        | None -> ()

    let back () =
        if not (Transitions.in_progress()) then
            match history with
            | (chart_meta, rate) :: xs ->

                match SelectedChart.CACHE_DATA with
                | Some chart_meta -> forward_history <- (chart_meta, SelectedChart.rate.Value) :: forward_history
                | None -> ()

                history <- xs
                SelectedChart._rate.Set rate
                SelectedChart.change(chart_meta, LibraryContext.None, true)
            | _ -> ()

    let can_go_back() = (List.isEmpty >> not) history

    let forward () =
        if not (Transitions.in_progress()) then
            match forward_history with
            | (chart_meta, rate) :: xs ->

                match SelectedChart.CACHE_DATA with
                | Some chart_meta -> history <- (chart_meta, SelectedChart.rate.Value) :: history
                | None -> ()

                forward_history <- xs
                SelectedChart._rate.Set rate
                SelectedChart.change(chart_meta, LibraryContext.None, true)
            | _ -> ()

    let can_go_forward() = (List.isEmpty >> not) forward_history