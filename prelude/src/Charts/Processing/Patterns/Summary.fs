namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts

[<Json.AutoCodec>]
type PatternReport =
    {
        Patterns: PatternBreakdown list
        LNPercent: float32
        SVAmount: Time
        Category: ChartCategorisation
        Density10: float32</rate>
        Density25: float32</rate>
        Density50: float32</rate>
        Density75: float32</rate>
        Density90: float32</rate>
    }
    static member Default = 
        { 
            Patterns = []
            LNPercent = 0.0f
            SVAmount = 0.0f<ms>
            Category = ChartCategorisation.Default
            Density10 = 0.0f</rate>
            Density25 = 0.0f</rate>
            Density50 = 0.0f</rate>
            Density75 = 0.0f</rate>
            Density90 = 0.0f</rate>
        }

module PatternReport =

    let from_chart_uncached (chart: Chart) : PatternReport =
        let raw_data, core_patterns, specific_patterns = PatternFinder.find_patterns chart

        let breakdown =
            core_patterns
            |> Clustering.cluster_pattern_bpms
            |> Seq.filter (fun (_, info) -> info.BPM.Value >= 40<beat / minute / rate>)
            |> Breakdown.generate specific_patterns
            |> Seq.sortByDescending (fun x -> x.Amount)
            |> List.ofSeq

        let is_useless (pattern: PatternBreakdown) =
            breakdown
            |> Seq.exists (fun p ->
                p.Pattern = pattern.Pattern
                && p.Amount * 0.5f > pattern.Amount
                && p.Density75 > pattern.Density75
                && p.BPM > pattern.BPM
                && p.Mixed = pattern.Mixed
            )

        let patterns = breakdown |> List.filter (is_useless >> not)
        let sv_amount = Metrics.sv_time chart

        let sorted_densities = raw_data |> Seq.map _.Density |> Array.ofSeq |> Array.sort

        {
            Patterns = patterns
            LNPercent = Metrics.ln_percent chart
            SVAmount = sv_amount
            Category = Categorise.categorise_chart chart.Keys patterns sv_amount
            Density10 = Density.find_percentile sorted_densities 0.1f
            Density25 = Density.find_percentile sorted_densities 0.25f
            Density50 = Density.find_percentile sorted_densities 0.5f
            Density75 = Density.find_percentile sorted_densities 0.75f
            Density90 = Density.find_percentile sorted_densities 0.9f
        }

    let from_chart = from_chart_uncached |> cached
