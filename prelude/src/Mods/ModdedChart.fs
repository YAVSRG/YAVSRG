namespace Prelude.Mods

open Prelude
open Prelude.Charts

[<Struct>]
[<RequireQualifiedAccess>]
type ModStatus =
    /// Scores are suitable for online upload, personal bests, leaderboards
    | Ranked
    /// Scores are not suitable for leaderboards but may still be uploaded for "unranked leaderboards" and the like
    | Unranked
    /// Scores are not worthy of submitting online at all
    /// Mainly for (practice) mods that are so highly configurable that it's just not worth storing them all online
    | Offline
    /// Unstored = Scores should not be saved
    /// For mods that are in development or will otherwise change and corrupt scores if they were saved
    | Unstored

type ModdedChart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
        ModsSelected: Map<string, int64>
        ModsApplied: Map<string, int64>
        Status: ModStatus
    }

    member this.FirstNote: Time = this.Notes.[0].Time
    member this.LastNote: Time = this.Notes.[this.Notes.Length - 1].Time

    member this.ToNoteData() : NoteData =
        { Keys = this.Keys; Notes = this.Notes }

    member this.AsChart: Chart =
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

    static member OfChart(chart: Chart) : ModdedChartInternal =
        {
            Keys = chart.Keys
            Notes = chart.Notes
            SV = chart.SV
            BPM = chart.BPM
        }
