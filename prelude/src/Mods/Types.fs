namespace Prelude.Mods

open Prelude
open Prelude.Charts

(*
    Marker for status of mods.
        0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
        1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
        2 = Scores with this mod enabled should not be saved at all e.g. something experimental or still in development
*)

type ModStatus =
    | Ranked = 0
    | Unranked = 1
    | Unstored = 2

type ModdedChart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
        ModsSelected: Map<string, int>
        ModsApplied: Map<string, int>
        Status: ModStatus
    }
    member this.FirstNote = this.Notes.[0].Time
    member this.LastNote = this.Notes.[this.Notes.Length - 1].Time
    member this.AsChart : Chart =
        {
            Keys = this.Keys
            Notes = this.Notes
            BPM = this.BPM
            SV = this.SV
        }

/// Intermediate chart getting mods applied to it, when completed it becomes a ModdedChart
type ModdedChartInternal =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
    }
    static member OfChart (chart: Chart) =
        {
            Keys = chart.Keys
            Notes = chart.Notes
            SV = chart.SV
            BPM = chart.BPM
        }

type Mod =
    {
        Status: ModStatus
        /// Mods can have an integer "state" passed to them when they are applied - This allows some sub-behaviours within mods
        States: int
        /// If true, state should be a randomised seed
        RandomSeed: bool
        /// List of mod ids this mod cannot be used with
        Exclusions: string list
        /// Returns resulting chart + flag
        /// flag is true if the mod made meaningful changes to the chart and the mod should be considered 'applied'
        Apply: int -> ModdedChartInternal -> ModdedChartInternal * bool
        /// Short code representing the mod, to be abbreviated in places without room for the full names
        /// Language independent
        Shorthand: int -> string
    }
    static member internal Default =
        {
            Status = ModStatus.Unstored
            States = 1
            Exclusions = []
            RandomSeed = false
            Apply = (fun _ mc -> mc, false)
            Shorthand = fun _ -> "??"
        }