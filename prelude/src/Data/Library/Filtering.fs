namespace Prelude.Data.Library

open FParsec

// This file is parsing rules for filters, they are actually applied to charts in Sorting.fs

type FilterPart =
    | Equals of string * string
    | LessThan of string * float
    | MoreThan of string * float
    | String of string
    | Tag of string
    | Impossible

type Filter = FilterPart list

module Filter =

    let private string = " =<>#\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()

    let private quoted_string =
        between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy)

    let private word = string |>> String
    let private quoted = quoted_string |>> fun s -> String <| s.ToLower()

    let private equals =
        string .>>. (pchar '=' >>. (string <|> quoted_string)) |>> Equals

    let private less = string .>>. (pchar '<' >>. pfloat) |>> LessThan
    let private more = string .>>. (pchar '>' >>. pfloat) |>> MoreThan
    let private tag = pchar '#' >>. string |>> Tag

    let private filter =
        sepBy (attempt equals <|> attempt less <|> attempt more <|> attempt tag <|> quoted <|> word) spaces1
        .>> spaces

    let parse (str: string) =
        match run filter (str.Trim()) with
        | Success(x, _, _) -> x
        | Failure(f, _, _) -> [ Impossible ]

    let except_keywords = List.filter (function String _ -> false | Impossible -> false | _ -> true)