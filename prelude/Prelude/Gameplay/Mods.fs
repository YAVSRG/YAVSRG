namespace Prelude.Gameplay

open System
open System.Collections.Generic
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Editor
open Prelude.Editor.Filter
open Prelude.Gameplay.Score

type ModChart = {
        Keys: int
        Notes: TimeData<NoteRow>
        BPM: TimeData<BPM>
        SV: MultiTimeData<float32>
        ModsUsed: string list
    }
module ModChart =
    let create (chart: Chart) : ModChart = { Keys = chart.Keys; Notes = chart.Notes.Clone(); BPM = chart.BPM.Clone(); SV = chart.SV.Clone(); ModsUsed = [] }

module Mods = 
    (*
        Marker for status of mods.
            0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
            1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
            2 = Scores with this mod enabled should not be saved at all e.g. when testing/developing or autoplay
    *)
    type ModStatus = Ranked = 0 | Unranked = 1 | Unstored = 2

    (*
        Mods (Modifiers) are a set of optional effects that can be enabled for a chart before playing it
        They will change something about the chart that can vary gameplay in interesting ways
        Mods will have an integer "state" passed to them when they are applied - This allows some sub-behaviours within mods
        RandomSeed indicates that state should be randomised and is used to seed random behaviour for the mod
            (This number is then saved so the mod/score can be replicated for replay purposes)


        Rules to use this correctly
            - 'Check' and 'Apply_score' should not modify anything about the Chart passed
            - 'Apply' should not modify the header of the Chart passed

        todo: 'Apply' functions should return ModChart instead of unit to allow for mutability of key count.
    *)

    type Mod = {
        Status: ModStatus
        States: int
        RandomSeed: bool
        Exclusions: string list
        //Looks at a chart and returns true if applying the modifier will do anything, otherwise false
        //Used to hide enabled mods that would have no effect e.g. a mod that removes hold notes on a chart with no hold notes
        Check: int -> ModChart -> bool
        //Applies the modifier to the content of the chart. This is where note data, timing data, sv data should be edited to create the desired effects
        Apply: int -> ModChart -> ModChart
        //Applies the modifier to the hit data of the chart - This hit data maps out what notes need to be hit/what notes have been hit
        //This typically is unused - examples uses are marking all notes as hit perfectly in Autoplay or marking LN releases as not needing to be hit
        Apply_Score: int -> ModChart -> ScoreData -> unit
    }

    type ModState = Map<string, int>
    
    let modList = new Dictionary<string, Mod>()
    let registerMod id obj = modList.Add(id, obj)

    module ModState =

        let r = new System.Random()

        let getModName id = Localisation.localise ("mod." + id + ".name")
        let getModDesc id = Localisation.localise ("mod." + id + ".desc")

        let cycleState id (mods: ModState): ModState =
            if (mods.ContainsKey(id)) then
                let state = mods.[id] + 1
                if state = modList.[id].States || modList.[id].RandomSeed then Map.remove id mods
                else Map.add id state mods
            else
                let state = if modList.[id].RandomSeed then r.Next(modList.[id].States) else 0
                List.fold (fun m i -> Map.remove i m) (Map.add id state mods) modList.[id].Exclusions

        let enumerate (mods: ModState) = Map.toSeq mods |> Seq.map fst |> Seq.sort

        let enumerateApplicable (chart: ModChart) (mods: ModState) =
            enumerate mods |> Seq.choose (fun id -> if modList.[id].Check(mods.[id]) chart then Some (id, modList.[id], mods.[id]) else None)

        let filterApplicable (chart: ModChart) (mods: ModState) =
            Seq.fold (fun m (i, _, _) -> Map.add i mods.[i] m) Map.empty (enumerateApplicable chart mods)

    let defaultMod = { Status = ModStatus.Unstored; States = 1; Exclusions = []; RandomSeed = false; Check = (fun _ _ -> true); Apply = (fun _ -> id); Apply_Score = fun _ _ _ -> () }

    let private auto _ _ hitData =
        for (t, delta, hit): ScoreDataRow in hitData do
            for i = 0 to (Array.length hit - 1) do
                if hit.[i] = HitStatus.NotHit then hit.[i] <- HitStatus.Hit; delta.[i] <- 0.0f<ms>

    registerMod "auto"
        { defaultMod with
            Status = ModStatus.Unstored
            Apply_Score = auto
        }

    registerMod "mirror"
        { defaultMod with
            Status = ModStatus.Ranked
            Apply = fun _ mc -> { mc with Notes = mirror (-infinityf * 1.0f<ms>) (infinityf * 1.0f<ms>) mc.Keys mc.Notes }
        }

    registerMod "nosv"
        { defaultMod with
            Status = ModStatus.Unranked
            Check = fun _ mc -> mc.SV.IsEmpty()
            Apply = fun _ mc -> mc.SV.Clear(); mc
        }

    //todo: no ln (removes all lns)
    //todo: inverse (exclusion with no ln)
    //todo: no releases (exclusion with no ln)
    //todo: randomiser with seed

    (*
        Mod application pipeline
    *)

    let private applyMods (mods: ModState) (chart: Chart) : ModChart =
        let mutable modChart = ModChart.create chart
        for (name, m, c) in mods |> ModState.enumerateApplicable modChart do
            let mc = m.Apply c modChart
            modChart <- { mc with ModsUsed = mc.ModsUsed @ [name] }
        modChart

    let private applyModsToScoreData (mods: ModState) (modChart: ModChart) (scoreData: ScoreData) =
        for (name, m, c) in mods |> ModState.enumerateApplicable modChart do
            m.Apply_Score c modChart scoreData
        scoreData

    let getModChart (mods: ModState) (chart: Chart) : ModChart = applyMods mods chart
    let createScoreData (mods: ModState) (mc: ModChart) : ScoreData = applyModsToScoreData mods mc (notesToScoreData mc.Keys mc.Notes)
    let loadScoreData (mc: ModChart) (replay: string) : ScoreData = decompressScoreData replay mc.Keys mc.Notes

    let getModString(rate: float32, selectedMods: ModState) = 
        String.Join(", ", sprintf "%.2fx" rate :: (selectedMods |> ModState.enumerate |> Seq.map (ModState.getModName) |> List.ofSeq))