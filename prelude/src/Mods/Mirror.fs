namespace Prelude.Mods

open Prelude

module Mirror =

    let apply (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        { chart with
            Notes = TimeArray.map Array.rev chart.Notes
        },
        true