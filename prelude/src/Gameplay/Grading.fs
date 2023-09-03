namespace Prelude.Gameplay

open Percyqaz.Json

[<RequireQualifiedAccess>]
type Improvement<'T> =
    | FasterBetter of rate_increase: float32 * improvement: 'T
    | Faster of rate_increase: float32
    | Better of improvement: 'T
    | New
    | None

[<Json.AutoCodec>]
type PersonalBests<'T> = { Best: 'T * float32; Fastest: 'T * float32 }

module PersonalBests =

    let create (value: 'T, rate: float32) : PersonalBests<'T> = { Best = value, rate; Fastest = value, rate }

    let map (f: 'T -> 'U) ({ Best = best_overall, best_rate; Fastest = best_fastest, fastest_rate }: PersonalBests<'T>) : PersonalBests<'U> =
        { Best = f best_overall, best_rate; Fastest = f best_fastest, fastest_rate }

    let inline update (value: 'T, rate: float32) ({ Best = best_overall, best_rate; Fastest = best_fastest, fastest_rate }: PersonalBests<'T>) =

        let new_fastest_best, f_rate_increase, f_improvement =
            if rate > fastest_rate then (value, rate), Some (rate - fastest_rate), None
            elif rate = fastest_rate then
                if value > best_fastest then (value, rate), None, Some (value - best_fastest)
                else (best_fastest, rate), None, None
            else (best_fastest, fastest_rate), None, None

        let new_overall_best, b_rate_increase, b_improvement =
            if value > best_overall then (value, rate), None, Some (value - best_overall)
            elif value = best_overall then
                if rate > best_rate then (value, rate), Some (rate - best_rate), None
                else (best_overall, best_rate), None, None
            else (best_overall, best_rate), None, None

        let result = { Best = new_overall_best; Fastest = new_fastest_best }
        let info =

            let rate_increase =
                match f_rate_increase, b_rate_increase with
                | Some f, Some b -> Some (max f b)
                | Some f, None -> Some f
                | None, Some b -> Some b
                | None, None -> None
            
            let improvement =
                match f_improvement, b_improvement with
                | Some f, Some b -> Some (max f b)
                | Some f, None -> Some f
                | None, Some b -> Some b
                | None, None -> None

            match rate_increase, improvement with
            | Some r, Some i -> Improvement.FasterBetter (r, i)
            | Some r, None -> Improvement.Faster r
            | None, Some i -> Improvement.Better i
            | None, None -> Improvement.None

        result, info

    let best_this_rate (rate: float32) ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) : 'T option =
        if r1 < rate then
            if r2 < rate then None else Some p2
        else Some p1

// todo: to be implemented soon
type PersonalBestsV2<'T> = (float32 * 'T) list

module PersonalBestsV2 =

    let rec get (minimum_rate: float32) (bests: PersonalBestsV2<'T>) =
        match bests with
        | [] -> None
        | (rate, value) :: xs -> if rate <= minimum_rate then Some value else get minimum_rate xs

    let inline add (rate: float32, value: 'T) (bests: PersonalBestsV2<'T>) : PersonalBestsV2<'T> * Improvement<'T> =
        let rec remove_worse_breakpoints (v: 'T) (bests: PersonalBestsV2<'T>) =
            match bests with
            | [] -> []
            | (_, value) :: xs when value <= v -> remove_worse_breakpoints v xs
            | xs -> xs
        let rec loop (xs: PersonalBestsV2<'T>) : PersonalBestsV2<'T> * Improvement<'T> =
            match xs with
            | [] -> (rate, value) :: [], Improvement.New
            | (r, v) :: xs ->
                if rate < r then
                    let res, imp = loop xs in (r, v) :: res, imp
                elif rate = r && value > v then
                    (rate, value) :: remove_worse_breakpoints value xs, Improvement.Better (value - v)
                elif rate = r then
                    (r, v) :: xs, Improvement.None
                else
                    if value > v then (rate, value) :: remove_worse_breakpoints value xs, Improvement.FasterBetter(rate - r, value - v)
                    elif value = v then (rate, value) :: remove_worse_breakpoints value xs, Improvement.Faster(rate - r)
                    else (rate, value) :: (r, v) :: xs, Improvement.New
        loop bests