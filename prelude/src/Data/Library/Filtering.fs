namespace Prelude.Data.Library

open FParsec

// This file is parsing rules for filters, they are actually applied to charts in Sorting.fs

type FilterPart =
    | Equals of string * string
    | NotEquals of string * string
    | LessThan of string * float
    | MoreThan of string * float
    | Clamp of string * float
    | String of string
    | NotString of string
    | Tag of string
    | Impossible

type Filter = FilterPart list

module Filter =

    let private string = " !=<>#~\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()

    let private quoted_string =
        between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy)

    let private word = string |>> String
    let private quoted = quoted_string |>> fun s -> String <| s.ToLower()
    let private antiquoted = pchar '!' >>. quoted_string |>> fun s -> NotString <| s.ToLower()

    let private equals =
        string .>>. (pchar '=' >>. (string <|> quoted_string)) |>> Equals

    let private notequals =
        string .>>. (pchar '!' >>. pchar '=' >>. (string <|> quoted_string)) |>> NotEquals

    let private less = string .>>. (pchar '<' >>. optional (pchar '=') >>. pfloat) |>> LessThan
    let private more = string .>>. (pchar '>' >>. optional (pchar '=') >>. pfloat) |>> MoreThan
    let private clamp = string .>>. (pchar '~' >>. pfloat) |>> Clamp
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
        | Success(x, _, _) -> x |> Percyqaz.Common.Combinators.debug
        | Failure(f, _, _) -> [ Impossible ]

    let except_keywords = List.filter (function String _ -> false | Impossible -> false | _ -> true)