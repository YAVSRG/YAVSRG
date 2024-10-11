namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts

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
        }

module PatternReport =

    let from_chart_uncached (chart: Chart) : PatternReport =
        let density, patterns = PatternFinder.find_patterns chart
        let clusters = 
            Clustering.calculate_clustered_patterns patterns
            |> Array.sortByDescending (fun x -> x.Amount)

        let can_be_pruned (cluster: Cluster) =
            clusters
            |> Percyqaz.Common.Combinators.debug
            |> Seq.exists (fun other ->
                other.Pattern = cluster.Pattern
                && (other.SpecificType.IsNone || other.SpecificType = cluster.SpecificType)
                && other.Amount * 0.5f > cluster.Amount
                && other.Density75 > cluster.Density75
                && other.BPM > cluster.BPM
            )

        let pruned_clusters = clusters |> Array.filter (can_be_pruned >> not)
        let sv_amount = Metrics.sv_time chart

        let sorted_densities = density |> Array.sort

        {
            Clusters = pruned_clusters
            LNPercent = Metrics.ln_percent chart
            SVAmount = sv_amount
            Category = Categorise.categorise_chart chart.Keys pruned_clusters sv_amount
            Density10 = Clustering.find_percentile 0.1f sorted_densities
            Density25 = Clustering.find_percentile 0.25f sorted_densities
            Density50 = Clustering.find_percentile 0.5f sorted_densities
            Density75 = Clustering.find_percentile 0.75f sorted_densities
            Density90 = Clustering.find_percentile 0.9f sorted_densities
        }

    let from_chart = from_chart_uncached |> cached
