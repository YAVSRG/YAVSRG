namespace Prelude.Gameplay

open System
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Tools

module Mods =
    (*
        Marker for status of mods.
            0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
            1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
            2 = Scores with this mod enabled should not be saved at all e.g. when testing/developing or autoplay
    *)
    type ModStatus =
        | Ranked = 0
        | Unranked = 1
        | Unstored = 2

    type ModState = Map<string, int>

    (*
        Mods (Modifiers) are a set of optional effects that can be enabled for a chart before playing it
        They will change something about the chart that can vary gameplay in interesting ways
        Mods will have an integer "state" passed to them when they are applied - This allows some sub-behaviours within mods
        RandomSeed indicates that state should be randomised and is used to seed random behaviour for the mod
            (This number is then saved so the mod/score can be replicated for replay purposes)

        Sanity rules
            - 'Check' should not modify anything about the Chart passed
            - 'Apply' should not modify the header of the Chart passed
    *)

    type Mod =
        {
            Status: ModStatus
            States: int
            RandomSeed: bool
            Exclusions: string list
            // Returns resulting chart + flag
            // flag is true if the mod made meaningful changes to the chart
            // The resulting chart can still be modified in ways that don't affect gameplay such as changing BPMs
            Apply: int -> ModChart -> ModChart * bool
            Priority: int
        }

    let available_mods = new Dictionary<string, Mod>()
    let add_mod id obj = available_mods.Add(id, obj)

    module ModState =

        let filter (chart: ModChart) (mods: ModState) =
            List.fold (fun m i -> Map.add i mods.[i] m) Map.empty chart.ModsUsed

        let r = new Random()

        let get_mod_name id =
            Localisation.localise ("mod." + id + ".name")

        let get_mod_desc id =
            Localisation.localise ("mod." + id + ".desc")

        let cycle id (mods: ModState) : ModState =
            if (mods.ContainsKey id) then
                let state = mods.[id] + 1

                if state = available_mods.[id].States || available_mods.[id].RandomSeed then
                    Map.remove id mods
                else
                    Map.add id state mods
            else
                let state =
                    if available_mods.[id].RandomSeed then
                        r.Next(available_mods.[id].States)
                    else
                        0

                List.fold (fun m i -> Map.remove i m) (Map.add id state mods) available_mods.[id].Exclusions

        let enumerate (mods: ModState) =
            mods
            |> Map.toSeq
            |> Seq.choose (fun (id, state) ->
                if available_mods.ContainsKey id then
                    Some(id, available_mods.[id], state)
                else
                    Logging.Error(sprintf "Unrecognised mod id: %s" id)
                    None
            )
            |> Seq.sortBy (fun (id, m, state) -> m.Priority)

    let private EMPTY_MOD =
        {
            Status = ModStatus.Unstored
            States = 1
            Exclusions = []
            RandomSeed = false
            Apply = (fun _ mc -> mc, false)
            Priority = 0
        }

    add_mod
        "mirror"
        { EMPTY_MOD with
            Status = ModStatus.Ranked
            Apply = fun _ mc -> Mirror.apply mc
        }

    add_mod
        "nosv"
        { EMPTY_MOD with
            Status = ModStatus.Unranked
            Apply = fun _ mc -> NoSV.apply mc
        }

    add_mod
        "noln"
        { EMPTY_MOD with
            Status = ModStatus.Unranked
            Exclusions = [ "inverse" ]
            Apply = fun _ mc -> NoLN.apply mc
        }

    add_mod
        "inverse"
        { EMPTY_MOD with
            Status = ModStatus.Unranked
            States = 1
            Exclusions = [ "noln" ]
            Apply = fun s mc -> Inverse.apply (s > 0) mc
        }

    // todo: randomiser mod with seed

    (*
        Mod application pipeline
    *)

    let apply_mods (mods: ModState) (chart: Chart) : ModChart =
        let mutable modchart = ModChart.from_chart chart

        for id, m, state in ModState.enumerate mods do
            let mc, mod_was_applied = m.Apply state modchart

            modchart <-
                { mc with
                    ModsUsed =
                        if mod_was_applied then
                            modchart.ModsUsed @ [ id ]
                        else
                            modchart.ModsUsed
                }

        modchart

    let format_mods (rate: float32, mods: ModState, autoplay: bool) =
        String.Join(
            ", ",
            sprintf "%.2fx" rate
            :: (mods
                |> ModState.enumerate
                |> Seq.map (fun (id, _, _) -> id)
                |> Seq.map (ModState.get_mod_name)
                |> List.ofSeq)
        )
        + if autoplay then ", " + ModState.get_mod_name "auto" else ""

    let check (mods: ModState) =
        try
            let mutable status = ModStatus.Ranked

            for m in mods.Keys do
                if available_mods.ContainsKey m then
                    status <- max status available_mods.[m].Status

                    if mods.[m] >= available_mods.[m].States then
                        failwithf "Mod '%s' in invalid state %i" m mods.[m]

                    for e in available_mods.[m].Exclusions do
                        if mods.ContainsKey e then
                            failwithf "Mods '%s' and '%s' cannot both be selected" m e
                else
                    failwithf "No such mod '%s'" m

            Ok status
        with err ->
            Error err.Message
