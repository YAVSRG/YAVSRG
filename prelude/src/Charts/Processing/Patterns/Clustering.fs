namespace Prelude.Charts.Processing.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Charts

type private BPMCluster =
    {
        mutable SumMs: float32<ms / beat>
        mutable OriginalMsPerBeat: float32<ms / beat>
        mutable Count: int
        mutable BPM: int option
    }
    member this.Add value =
        this.Count <- this.Count + 1
        this.SumMs <- this.SumMs + value

    member this.Calculate() =
        let average = this.SumMs / float32 this.Count
        this.BPM <- 60000.0f<ms / minute> / average |> float32 |> round |> int |> Some

    member this.Value = this.BPM.Value

type private BPMClusteredPattern =
    {
        Time: GameplayTime
        BPM: BPMCluster
        Density: float32
        Mixed: bool
    }

module private Clustering =

    let BPM_CLUSTER_THRESHOLD = 5.0f<ms / beat>

    let cluster_pattern_bpms
        (matched_patterns: MatchedCorePattern array)
        : (CorePatternType * BPMClusteredPattern) array =
        let clusters = ResizeArray<BPMCluster>()
        let mixed_clusters = Dictionary<CorePatternType, BPMCluster>()

        let get_cluster value : BPMCluster =
            match
                clusters
                |> Seq.tryFind (fun c -> abs (c.OriginalMsPerBeat - value) < BPM_CLUSTER_THRESHOLD)
            with
            | Some existing_cluster ->
                existing_cluster.Add value
                existing_cluster
            | None ->
                let new_cluster =
                    {
                        Count = 1
                        SumMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                clusters.Add new_cluster
                new_cluster

        let get_mixed_cluster pattern value : BPMCluster =
            if mixed_clusters.ContainsKey pattern then
                let existing_cluster = mixed_clusters.[pattern]
                existing_cluster.Add value
                existing_cluster
            else
                let new_cluster =
                    {
                        Count = 1
                        SumMs = value
                        OriginalMsPerBeat = value
                        BPM = None
                    }

                mixed_clusters.Add(pattern, new_cluster)
                new_cluster

        let result =
            matched_patterns
            |> Array.map (fun info ->
                (info.Pattern,
                 {
                     Time = info.Time
                     BPM =
                         if info.Mixed then
                             get_mixed_cluster info.Pattern info.MsPerBeat
                         else
                             get_cluster info.MsPerBeat
                     Density = info.Density
                     Mixed = info.Mixed
                 })
            )

        clusters |> Seq.iter (fun cluster -> cluster.Calculate())
        mixed_clusters.Values |> Seq.iter (fun cluster -> cluster.Calculate())

        result