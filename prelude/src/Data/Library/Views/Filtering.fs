namespace Prelude.Data.Library

open System
open FParsec
open Prelude
open Prelude.Charts.Processing.Patterns

type FilterPart =
    | Equals of string * string
    | NotEquals of string * string
    | LessThan of string * float32
    | MoreThan of string * float32
    | Clamp of string * float32
    | String of string
    | NotString of string
    | Tag of string
    | Impossible

module FilterParts =

    let private string = " !=<>#~\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLowerInvariant()

    let private quoted_string =
        between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy)

    let private word = string |>> String
    let private quoted = quoted_string |>> fun s -> String <| s.ToLowerInvariant()
    let private antiquoted = pchar '!' >>. quoted_string |>> fun s -> NotString <| s.ToLowerInvariant()

    let private equals =
        string .>>. (pchar '=' >>. (string <|> quoted_string)) |>> (fun (k, v) -> Equals(k.ToLowerInvariant(), v.ToLowerInvariant()))

    let private notequals =
        string .>>. (pchar '!' >>. pchar '=' >>. (string <|> quoted_string)) |>> NotEquals

    let private pfloat32 = pfloat |>> float32

    let private less = string .>>. (pchar '<' >>. optional (pchar '=') >>. pfloat32) |>> LessThan
    let private more = string .>>. (pchar '>' >>. optional (pchar '=') >>. pfloat32) |>> MoreThan
    let private clamp = string .>>. (pchar '~' >>. pfloat32) |>> Clamp
    let private tag = pchar '#' >>. string |>> Tag

    let private filter =
        sepBy (
            attempt equals 
            <|> attempt notequals
            <|> attempt clamp
            <|> attempt less
            <|> attempt more
            <|> attempt tag
            <|> attempt antiquoted
            <|> quoted
            <|> word
        ) spaces1
        .>> spaces

    let parse (str: string) =
        match run filter (str.Trim()) with
        | Success(x, _, _) -> x
        | Failure(f, _, _) -> [ Impossible ]

type Filter =
    internal {
        SearchTerms: string array
        SearchAntiTerms: string array
        PatternTerms: string array
        PatternAntiTerms: string array

        BPMClamp: (CorePatternType * float) option

        Keymode: int option
        LengthMin: float32 option
        LengthMax: float32 option
        DifficultyMin: float32 option
        DifficultyMax: float32 option
        LNPercentMin: float32 option
        LNPercentMax: float32 option
        SV: bool option
    }

    static member Empty =
        {
            SearchTerms = [||]
            SearchAntiTerms = [||]
            PatternTerms = [||]
            PatternAntiTerms = [||]

            BPMClamp = None

            Keymode = None
            LengthMin = None
            LengthMax = None
            DifficultyMin = None
            DifficultyMax = None
            LNPercentMin = None
            LNPercentMax = None
            SV = None
        }

    member this.WithoutSearchTerms =
        { this with
            SearchTerms = [||]
            SearchAntiTerms = [||]
        }

    member this.Compile(ctx: LibraryViewContext) : (ChartMeta -> bool) array =
        seq {
            match this.Keymode with
            | Some k -> yield fun cc -> cc.Keys = k
            | None -> ()

            match this.LengthMin with
            | Some min_length -> yield fun cc -> cc.Length / 1000.0f<ms> >= min_length
            | None -> ()
            match this.LengthMax with
            | Some max_length -> yield fun cc -> cc.Length / 1000.0f<ms> <= max_length
            | None -> ()

            match this.DifficultyMin with
            | Some min_diff -> yield fun cc -> cc.Rating >= min_diff
            | None -> ()
            match this.DifficultyMax with
            | Some max_diff -> yield fun cc -> cc.Rating <= max_diff
            | None -> ()

            match this.LNPercentMin with
            | Some min_pc -> yield fun cc -> cc.Patterns.LNPercent > min_pc
            | None -> ()
            match this.LNPercentMax with
            | Some max_pc -> yield fun cc -> cc.Patterns.LNPercent < max_pc
            | None -> ()

            match this.SV with
            | Some false -> yield fun cc -> not (cc.Patterns.SVAmount > Categorise.SV_AMOUNT_THRESHOLD)
            | Some true -> yield fun cc -> cc.Patterns.SVAmount > Categorise.SV_AMOUNT_THRESHOLD
            | None -> ()

            if this.SearchTerms.Length <> 0 || this.SearchAntiTerms.Length <> 0 then
                yield fun cc ->
                    let s =
                        (cc.Title
                            + " "
                            + cc.Artist
                            + " "
                            + cc.Creator
                            + " "
                            + cc.DifficultyName
                            + " "
                            + (cc.Subtitle |> Option.defaultValue "")
                            + " "
                            + String.concat " " cc.Folders)
                            .ToLowerInvariant()
                    Array.forall (s.Contains : string -> bool) this.SearchTerms
                    && Array.forall (s.Contains >> not : string -> bool) this.SearchAntiTerms

            if this.PatternTerms.Length <> 0 || this.PatternAntiTerms.Length <> 0 then
                yield fun cc ->
                    let report = cc.Patterns

                    let matches (pattern: string) =
                        report.Category.Category.Contains(pattern, StringComparison.OrdinalIgnoreCase)
                        || (report.Category.MajorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))
                        || (report.Category.MinorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))

                    Array.forall matches this.PatternTerms
                    && Array.forall (matches >> not) this.PatternAntiTerms
        }
        |> Array.ofSeq

    static member FromParts(parts: FilterPart list) : Filter =
        let mutable filter = Filter.Empty

        for p in parts do
            match p with
            | Impossible -> filter <- { filter with SearchAntiTerms = [|""|] }
            | String str -> filter <- { filter with SearchTerms = Array.append filter.SearchTerms [| str |] }
            | NotString str -> filter <- { filter with SearchAntiTerms = Array.append filter.SearchAntiTerms [| str |] }

            | Equals("p", str)
            | Equals("pattern", str) -> filter <- { filter with PatternTerms = Array.append filter.PatternTerms [| str |] }
            | NotEquals("p", str)
            | NotEquals("pattern", str) -> filter <- { filter with PatternAntiTerms = Array.append filter.PatternAntiTerms [| str |] }

            | Equals("k", n)
            | Equals("key", n)
            | Equals("keys", n) -> 
                match Int32.TryParse(n) with
                | true, k -> filter <- { filter with Keymode = Some k }
                | false, _ -> ()

            | MoreThan("d", d)
            | MoreThan("diff", d) -> filter <- { filter with DifficultyMin = Some d }
            | LessThan("d", d)
            | LessThan("diff", d) -> filter <- { filter with DifficultyMax = Some d }

            | MoreThan("l", l)
            | MoreThan("length", l) -> filter <- { filter with LengthMin = Some l }
            | LessThan("l", l)
            | LessThan("length", l) -> filter <- { filter with LengthMax = Some l }

            | LessThan("ln", pc)
            | LessThan("holds", pc)
            | LessThan("lns", pc) -> filter <- { filter with LNPercentMin = Some pc }
            | MoreThan("ln", pc)
            | MoreThan("holds", pc)
            | MoreThan("lns", pc) -> filter <- { filter with LNPercentMax = Some pc }

            | Tag "nosv"
            | Tag "nsv" -> filter <- { filter with SV = Some false }
            | Tag "sv" -> filter <- { filter with SV = Some true }

            | _ -> ()
        filter

module Filter =

    let private apply (compiled_filter: (ChartMeta -> bool) array) (cc: ChartMeta) : bool =
        Array.forall (fun f -> f cc) compiled_filter

    let apply_seq (filter: Filter, ctx: LibraryViewContext) (charts: ChartMeta seq) =
        Seq.filter (apply (filter.Compile ctx)) charts

    let apply_ctx_seq (filter: Filter, ctx: LibraryViewContext) (charts: (ChartMeta * 'T) seq) =
        Seq.filter (fun (cc, _) -> apply (filter.Compile ctx) cc) charts