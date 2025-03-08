namespace Prelude.Calculator.Patterns

open Prelude

module Categorise =

    let SV_AMOUNT_THRESHOLD = 2000.0f<ms>

    let categorise_chart (keys: int, ordered_clusters: Cluster array, sv_amount: Time) : string =

        if ordered_clusters.Length = 0 then
            if sv_amount >= SV_AMOUNT_THRESHOLD then "SV" else "Uncategorised"
        else

        let important_clusters =
            ordered_clusters |> Array.takeWhile (fun c -> c.Importance / ordered_clusters.[0].Importance > 0.5f)

        let cluster_1 = Array.item 0 important_clusters
        let cluster_2 = Array.tryItem 1 important_clusters

        let is_hybrid =
            match cluster_2 with
            | Some { Pattern = Jacks } when cluster_1.Pattern = Stream -> true
            | Some { Pattern = Jacks } when cluster_1.Pattern = Chordstream -> true
            | Some { Pattern = Stream } when cluster_1.Pattern = Jacks -> true
            | Some { Pattern = Chordstream } when cluster_1.Pattern = Jacks -> true
            | _ -> false

        let is_tech = cluster_1.Mixed

        let is_sv = sv_amount >= SV_AMOUNT_THRESHOLD

        let name =
            match cluster_1.SpecificTypes with
            | (main_type, amount) :: _ when amount > 0.4f -> main_type
            | ("Jumpstream", a1) :: ("Handstream", a2) :: _ when a2 / a1 > 0.4f -> "Jumpstream/Handstream"
            | _ -> cluster_1.Pattern.ToString()

        sprintf "%s%s%s%s" name (if is_hybrid then " Hybrid" else "") (if is_tech then " Tech" else "") (if is_sv then " + SV" else "")