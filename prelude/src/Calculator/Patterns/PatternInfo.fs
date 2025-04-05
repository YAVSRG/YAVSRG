namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts
open Prelude.Calculator

// todo: wonder about making LN sections separate to the main pattern
// 7k files like to have a big LN spam at the end that should register separately
type PatternCluster =
    {
        Pattern: CorePattern
        BPM: int<beat / minute>
        Mixed: bool
        Amount: Time
        Difficulty: float32

        LN10: float32
        LN25: float32
        LN50: float32
        LN75: float32
        LN90: float32

        Variety10: float32
        Variety25: float32
        Variety50: float32
        Variety75: float32
        Variety90: float32

        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density
    }

/// Calculated dynamically for a specific chart + rate
/// Can have more details compared to the LibraryPatternInfo which is precalculated and stored for every chart
type PatternInfo =
    {
        Difficulty: float32
        SVAmount: Time
        Duration: Time

        MainPatterns: PatternCluster array
        HoldNotePercent: float32
        Purity: float32
        Simplicity: float32

        // have to stay for now for combined ratings - prepare to be destroyed!
        Density10: Density
        Density25: Density
        Density50: Density
        Density75: Density
        Density90: Density
    }

module PatternInfo =

    let from_chart_uncached (rate: Rate, chart: Chart) : PatternInfo =
        let difficulty = Difficulty.calculate (rate, chart.Notes)
        let density = Density.process_chart chart
        let patterns = Patterns.find (density, difficulty, chart)

        let clusters =
            Clustering.calculate_clustered_patterns patterns
            |> Array.filter (fun c -> c.BPM > 25<beat / minute / rate>)

        let sv_amount = Metrics.sv_time chart
        let sorted_densities = density |> Array.sort

        {
            Difficulty = difficulty.Overall
            SVAmount = sv_amount
            Duration = chart.LastNote - chart.FirstNote

            MainPatterns = [||]
            HoldNotePercent = Metrics.ln_percent chart
            Purity = 0.0f
            Simplicity = 0.0f
            Density10 = Clustering.find_percentile 0.1f sorted_densities
            Density25 = Clustering.find_percentile 0.25f sorted_densities
            Density50 = Clustering.find_percentile 0.5f sorted_densities
            Density75 = Clustering.find_percentile 0.75f sorted_densities
            Density90 = Clustering.find_percentile 0.9f sorted_densities

        }

    let from_chart = from_chart_uncached |> cached