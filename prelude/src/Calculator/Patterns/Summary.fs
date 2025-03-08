namespace Prelude.Calculator.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Calculator

[<Json.AutoCodec>]
type PatternReport =
    {
        Clusters: Cluster array
        Category: string
        LNPercent: float32
        SVAmount: Time

        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density

        Duration: Time
    }
    static member Default =
        {
            Clusters = [||]
            LNPercent = 0.0f
            SVAmount = 0.0f<ms>
            Category = "Unknown"

            Density10 = 0.0f</rate>
            Density25 = 0.0f</rate>
            Density50 = 0.0f</rate>
            Density75 = 0.0f</rate>
            Density90 = 0.0f</rate>

            Duration = 0.0f<ms>
        }
    member this.ImportantClusters =
        match Array.tryHead this.Clusters with
        | None -> Seq.empty
        | Some c ->

        let importance = c.Importance
        this.Clusters |> Seq.takeWhile (fun c -> c.Importance / importance > 0.5f)

module PatternReport =

    let from_chart_uncached (difficulty_info: Difficulty, chart: Chart) : PatternReport =
        let density = Density.process_chart chart
        let patterns = Patterns.find (density, difficulty_info, chart)
        let clusters =
            Clustering.calculate_clustered_patterns patterns
            |> Seq.filter (fun c -> c.BPM > 25<beat / minute / rate>)
            |> Seq.sortByDescending (fun x -> x.Amount)
            |> Array.ofSeq

        let can_be_pruned (cluster: Cluster) =
            clusters
            |> Seq.exists (fun other ->
                other.Pattern = cluster.Pattern
                && other.Amount * 0.5f > cluster.Amount
                && other.BPM > cluster.BPM
            )

        let pruned_clusters =
            seq {
                let clusters = clusters |> Seq.filter (can_be_pruned >> not) |> Array.ofSeq
                yield! clusters |> Seq.filter (fun x -> x.Pattern = Stream) |> Seq.truncate 3
                yield! clusters |> Seq.filter (fun x -> x.Pattern = Chordstream) |> Seq.truncate 3
                yield! clusters |> Seq.filter (fun x -> x.Pattern = Jacks) |> Seq.truncate 3
            }
            |> Seq.sortByDescending (fun x -> x.Importance)
            |> Array.ofSeq

        let sv_amount = Metrics.sv_time chart
        let sorted_densities = density |> Array.sort

        {
            Clusters = pruned_clusters
            LNPercent = Metrics.ln_percent chart
            SVAmount = sv_amount
            Category = Categorise.categorise_chart (chart.Keys, pruned_clusters, sv_amount)
            Density10 = Clustering.find_percentile 0.1f sorted_densities
            Density25 = Clustering.find_percentile 0.25f sorted_densities
            Density50 = Clustering.find_percentile 0.5f sorted_densities
            Density75 = Clustering.find_percentile 0.75f sorted_densities
            Density90 = Clustering.find_percentile 0.9f sorted_densities

            Duration = chart.LastNote - chart.FirstNote
        }

    let from_chart = from_chart_uncached |> cached