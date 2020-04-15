namespace Prelude.Gameplay

open System.Collections.Generic
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Editor
open Prelude.Editor.Filter
open Prelude.Gameplay.Score

module Mods = 
    type ModConfig = int
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

        Rules to use this correctly
            - 'check' and 'apply_score' should not modify anything about the Chart passed
            - 'apply' should not modify the header of the Chart passed

        todo: 'apply' functions should return ModChart instead of unit to allow for mutability of key count.
    *)
    type Mod
        (status : ModStatus, numStates : int, check : ModConfig -> ModChart -> bool,
            apply : ModConfig -> ModChart -> unit, apply_score : ModConfig -> ModChart -> ScoreData -> unit) = 
        member this.MaxState = numStates
        member this.Status = status
        //Looks at a chart and returns true if applying the modifier will do anything, otherwise false
        //Used to hide enabled mods that would have no effect e.g. a mod that removes hold notes on a chart with no hold notes
        member this.CheckApplicable = check
        //Applies the modifier to the content of the chart. This is where note data, timing data, sv data should be edited to create the desired effects
        member this.ApplyChart = apply
        //Applies the modifier to the hit data of the chart - This hit data maps out what notes need to be hit/what notes have been hit
        //This typically is unused - examples uses are marking all notes as hit perfectly in Autoplay or marking LN releases as not needing to be hit
        member this.ApplyHitData = apply_score

    type ModState(mods : Dictionary<string, int>) =
        static let modList = new Dictionary<string, Mod>();
    
        new() = ModState(new Dictionary<string, int> ())

        member this.EnableMod id state = 
            if (modList.ContainsKey(id)) then
                if modList.[id].MaxState >= state then
                    mods.[id] <- state
                else failwith ("tried to assign an invalid state to " + string id + ": " + string state)
            else failwith ("tried to enable a mod that does not exist: " + string id)

        member this.DisableMod id = mods.Remove(id)

        member this.IterApplicable chart =
            seq {
                for id in mods.Keys do
                    if modList.[id].CheckApplicable (mods.[id]) chart then yield (id, modList.[id], mods.[id])
                }

        static member ModList = modList
        static member RegisterMod id obj = modList.Add(id, obj)
        static member GetModName id = localise ("mod." + id + ".name")
        static member GetModDesc id = localise ("mod." + id + ".desc")

    let private auto _ _ hitData =
        for (t, delta, hit) in hitData do
            for i = 0 to (Array.length hit - 1) do
                if hit.[i] = HitStatus.NotHit then hit.[i] <- HitStatus.Hit

    ModState.RegisterMod "auto"
        (Mod(ModStatus.Unstored, 0, (fun _ _ -> true), (fun _ _ -> ()), auto))

    ModState.RegisterMod "mirror"
        (Mod(ModStatus.Ranked, 0, (fun _ _ -> true), (fun _ (keys, notes, _, _, _) -> mirror -infinity infinity keys notes |> ignore), (fun _ _ _ -> ())))

    ModState.RegisterMod "nosv"
        (Mod(ModStatus.Ranked, 0, (fun _ (_, _, _, sv, _) -> not sv.IsEmpty), (fun _ (_, _, _, sv, _) -> sv.Clear), (fun _ _ _ -> ())))

    (*
        Mod application pipeline
    *)

    let private applyMods (mods: ModState) (chart: Chart): ModChart =
        let mutable modChart = (chart.Keys, TimeData(chart.Notes), TimeData(chart.BPM), chart.SV.Clone, []): ModChart
        for (name, m, c) in mods.IterApplicable modChart do
            m.ApplyChart c modChart
            let (keys, notes, bpm, sv, m) = modChart in
                modChart <- (keys, notes, bpm, sv, m @ [name])
        modChart

    let private applyModsToScoreData (mods: ModState) (modChart: ModChart) (scoreData: ScoreData) =
        for (name, m, c) in mods.IterApplicable modChart do
            m.ApplyHitData c modChart scoreData
        scoreData

    let getModChart (mods: ModState) (chart: Chart): Lazy<ModChart> * Lazy<ScoreData> =
        let mc = lazy (applyMods mods chart)
        let scoreData = lazy (
            let (keys, notes, _, _, _) = mc.Force()
            applyModsToScoreData mods (mc.Force()) (notesToScoreData keys notes)
        )
        (mc, scoreData)

    let getModChartWithScore (mods: ModState) (chart: Chart) (replay: string): Lazy<ModChart> * Lazy<ScoreData> =
        let mc = lazy (applyMods mods chart)
        let scoreData = lazy (
            let (keys, notes, _, _, _) = mc.Force()
            (decompressScoreData replay keys notes)
        )
        (mc, scoreData)