﻿namespace Prelude.Mods

open System
open Prelude
open Prelude.Charts

type ModState = Map<string, int64>

module ModState =

    open Mods

    let private seed_generation = new Random()

    let cycle (id: string) (mods: ModState) : ModState =
        if mods.ContainsKey id then

            match AVAILABLE_MODS.[id].Type with
            | Stateless
            | RandomSeed
            | ColumnSwap -> Map.remove id mods
            | MultipleModes states ->
                let state = mods.[id] + 1L
                if state >= states then
                    Map.remove id mods
                else
                    Map.add id state mods
        else

            let state =
                match AVAILABLE_MODS.[id].Type with
                | Stateless
                | MultipleModes _ -> 0L
                | RandomSeed -> (seed_generation.Next(-Int32.MinValue,0))
                | ColumnSwap ->
                    ColumnSwap.parse "1234123"
                    |> Percyqaz.Common.Combinators.expect
                    |> ColumnSwap.pack

            List.fold (fun m i -> Map.remove i m) (Map.add id state mods) AVAILABLE_MODS.[id].Exclusions

    let cycle_column_swap (id: string) (columns: int array) (mods: ModState) : ModState =
        if mods.ContainsKey id then
            Map.remove id mods
        else
            List.fold (fun m i -> Map.remove i m) (Map.add id (ColumnSwap.pack columns) mods) AVAILABLE_MODS.[id].Exclusions

    let in_priority_order (mods: ModState) : (string * Mod * int64) seq =
        APPLICATION_PRIORITY_ORDER
        |> Seq.choose (fun id ->
            match Map.tryFind id mods with
            | Some state -> Some (id, AVAILABLE_MODS.[id], state)
            | None -> None
        )

    let apply (mods: ModState) (chart: Chart) : ModdedChart =
        let mutable modchart_internal = ModdedChartInternal.OfChart chart

        let mutable mods_applied = []
        let mutable status = ModStatus.Ranked

        for id, m, state in in_priority_order mods do
            let new_mc, mod_was_applied = m.Apply state modchart_internal

            if mod_was_applied then
                mods_applied <- mods_applied @ [ id ]
                modchart_internal <- new_mc
                status <- max status m.Status

        {
            Keys = modchart_internal.Keys
            Notes = modchart_internal.Notes
            SV = modchart_internal.SV
            BPM = modchart_internal.BPM

            ModsSelected = mods
            ModsApplied = List.fold (fun m i -> Map.add i mods.[i] m) Map.empty mods_applied
            Status = status
        }

    let format (rate: Rate, mods: ModState) : string =
        String.Join(
            ", ",
            sprintf "%.2fx" rate
            :: (mods
                |> in_priority_order
                |> Seq.map (fun (id, m, state) -> m.Shorthand state)
                |> List.ofSeq)
        )

    let check (mods: ModState) : Result<ModStatus, string> =
        try
            let mutable status = ModStatus.Ranked

            for m in mods.Keys do
                if AVAILABLE_MODS.ContainsKey m then
                    status <- max status AVAILABLE_MODS.[m].Status

                    match AVAILABLE_MODS.[m].Type with
                    | Stateless ->
                        if mods.[m] <> 0L then failwithf "Mod '%s' in invalid state %i" m mods.[m]
                    | RandomSeed -> ()
                    | ColumnSwap -> ColumnSwap.unpack mods.[m] |> ignore
                    | MultipleModes states ->
                        if mods.[m] < 0L || mods.[m] >= states then
                            failwithf "Mod '%s' in invalid state %i" m mods.[m]

                    for e in AVAILABLE_MODS.[m].Exclusions do
                        if mods.ContainsKey e then
                            failwithf "Mods '%s' and '%s' cannot both be selected" m e
                else
                    failwithf "No such mod '%s'" m

            Ok status
        with err ->
            Error err.Message