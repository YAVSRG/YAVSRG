namespace Prelude.Mods

open System
open Prelude

module NoSV =

    let apply (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let mutable has_sv = false

        for { Data = s } in chart.SV do
            if MathF.Round(s, 2) <> 1.0f then
                has_sv <- true

        { chart with SV = [||] }, has_sv