namespace Prelude.Gameplay

open System
open System.Collections.Generic
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools

module Mods = 
    (*
        Marker for status of mods.
            0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
            1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
            2 = Scores with this mod enabled should not be saved at all e.g. when testing/developing or autoplay
    *)
    type ModStatus = Ranked = 0 | Unranked = 1 | Unstored = 2

    type ModState = Map<string, int>
    type ModChart =
        {
            Keys: int
            Notes: TimeData<NoteRow>
            BPM: TimeData<BPM>
            SV: MultiTimeData<float32>
            ModsUsed: string list
        }

    module ModChart =
        let create (chart: Chart) : ModChart = { Keys = chart.Keys; Notes = chart.Notes.Clone(); BPM = chart.BPM.Clone(); SV = chart.SV.Clone(); ModsUsed = [] }
        
        let filter (chart: ModChart) (mods: Map<string, int>) =
            List.fold (fun m i -> Map.add i mods.[i] m) Map.empty chart.ModsUsed

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
            // Returns flag + resulting chart
            // flag is true if the mod made meaningful changes to the chart
            // The resulting chart can still be modified in ways that don't affect gameplay such as changing BPMs
            Apply: int -> ModChart -> bool * ModChart
            Priority: int
        }
    
    let modList = new Dictionary<string, Mod>()
    let registerMod id obj = modList.Add (id, obj)

    module ModState =

        let r = new Random()

        let getModName id = Localisation.localise ("mod." + id + ".name")
        let getModDesc id = Localisation.localise ("mod." + id + ".desc")

        let cycleState id (mods: ModState) : ModState =
            if (mods.ContainsKey id) then
                let state = mods.[id] + 1
                if state = modList.[id].States || modList.[id].RandomSeed then Map.remove id mods
                else Map.add id state mods
            else
                let state = if modList.[id].RandomSeed then r.Next(modList.[id].States) else 0
                List.fold (fun m i -> Map.remove i m) (Map.add id state mods) modList.[id].Exclusions

        let enumerate (mods: ModState) = 
            mods
            |> Map.toSeq
            |> Seq.choose (fun (id, state) -> 
                    if modList.ContainsKey id then Some (id, modList.[id], state)
                    else Logging.Error(sprintf "Unrecognised mod id: %s" id); None
                )
            |> Seq.sortBy (fun (id, m, state) -> m.Priority)

    let defaultMod = { Status = ModStatus.Unstored; States = 1; Exclusions = []; RandomSeed = false; Apply = (fun _ mc -> false, mc); Priority = 0 }

    registerMod "mirror"
        { defaultMod with
            Status = ModStatus.Ranked
            Apply = fun _ mc -> true, { mc with Notes = Filter.mirror -Time.infinity Time.infinity mc.Keys mc.Notes }
        }

    registerMod "nosv"
        { defaultMod with
            Status = ModStatus.Unranked
            Apply = fun _ mc -> if mc.SV.IsEmpty() then false, mc else true, { mc with SV = new MultiTimeData<float32>(mc.Keys) }
        }

    registerMod "inverse"
        { defaultMod with
            Status = ModStatus.Unranked
            States = 1
            Apply = fun s mc -> true, { mc with Notes = Inverse.apply mc.Keys mc.BPM mc.Notes (s > 0) }
        }

    //todo: no ln (removes all lns)
    //todo: randomiser with seed

    (*
        Mod application pipeline
    *)

    let private applyMods (mods: ModState) (chart: Chart) : ModChart =
        let mutable modChart = ModChart.create chart

        for id, m, state in ModState.enumerate mods do
            let mod_was_applied, mc = m.Apply state modChart
            modChart <- { mc with ModsUsed = if mod_was_applied then mc.ModsUsed @ [id] else mc.ModsUsed }

        modChart

    let getModChart (mods: ModState) (chart: Chart) : ModChart = applyMods mods chart

    let getModString(rate: float32, selectedMods: ModState, autoPlay: bool) = 
        String.Join(", ", sprintf "%.2fx" rate :: (selectedMods |> ModState.enumerate |> Seq.map (fun (id, _, _) -> id) |> Seq.map (ModState.getModName) |> List.ofSeq))
        + if autoPlay then ", " + ModState.getModName "auto" else ""