namespace Prelude.Formats.StepMania

open System.IO
open FParsec
open Prelude

//https://github.com/stepmania/stepmania/wiki/sm
//https://github.com/stepmania/stepmania/wiki/ssc

module StepmaniaParser =

    // Main #KEY:VALUE; parser
    let private separator = anyOf ":;"

    let private token =
        let normal_char = noneOf ":;"
        let escaped_char = skipChar '\\' >>. anyChar
        manyChars (notFollowedBy (newline .>> skipChar '#') >>. (escaped_char <|> normal_char)) |>> fun s -> s.Trim()

    let private header_value = (spaces >>. (skipChar '#' >>. token .>> separator) .>>. (sepBy1 token separator) .>> skipRestOfLine true)

    let private header_value_or_discard_line =
        (header_value |>> Some) <|> (many1CharsTill anyChar (pchar '\n') >>% None)

    let private parse_header: Parser<Header, unit> =
        many header_value_or_discard_line
        |>> List.choose id
        .>> eof

    // Parse valid note rows

    let private parse_note_row = many ((many1Chars (anyOf "01234MLF")) .>> spaces)

    let private comment = optional (skipString "//" >>. skipRestOfLine true >>. spaces)

    let private parse_measures =
        (optional (spaces >>. comment .>> spaces))
        >>. (sepBy parse_note_row (pchar ',' .>> spaces .>> (optional (comment .>> spaces))))
        .>> eof

    // Parse BPMs and STOPs

    let private parse_pairs =
        (sepBy (pfloat .>> pchar '=' .>>. pfloat .>> spaces) (pchar ','))

    //https://github.com/etternagame/etterna/blob/master/src/Etterna/Singletons/GameManager.cpp
    let private parse_chart_type t =
        match t with
        | "dance-single" -> Dance_Single
        | "dance-double" -> Dance_Double
        | "dance-couple" -> Dance_Couple
        | "dance-solo" -> Dance_Solo
        | "dance-routine" -> Dance_Routine
        | "dance-threepanel" -> Dance_Threepanel
        | "pump-single" -> Pump_Single
        | "pump-double" -> Pump_Double
        | "pump-couple" -> Pump_Couple
        | "pump-halfdouble" -> Pump_Halfdouble
        | "pump-routine" -> Pump_Routine
        | "kb7-single" -> Kb7_Single
        | _ -> UNKNOWN

    let private read_stepmania_data header =
        let f s (key, values) =
            match key, values with
            | "TITLE", t :: _  when t <> "" -> { s with TITLE = t }
            | "SUBTITLE", t :: _ when t <> "" -> { s with SUBTITLE = t }
            | "ARTIST", t :: _ when t <> "" -> { s with ARTIST = t }
            | "TITLETRANSLIT", t :: _ when t <> "" -> { s with TITLETRANSLIT = t }
            | "SUBTITLETRANSLIT", t :: _ when t <> "" -> { s with SUBTITLETRANSLIT = t }
            | "ARTISTTRANSLIT", t :: _ when t <> "" -> { s with ARTISTTRANSLIT = t }
            | "GENRE", t :: _ when t <> "" -> { s with GENRE = t }
            | "CREDIT", t :: _ when t <> "" -> { s with CREDIT = t }
            | "BANNER", t :: _ when t <> "" -> { s with BANNER = t }
            | "BACKGROUND", t :: _ when t <> "" -> { s with BACKGROUND = t }
            | "CDTITLE", t :: _ when t <> "" -> { s with CDTITLE = t }
            | "MUSIC", t :: _ when t <> "" -> { s with MUSIC = t }
            | "OFFSET", v :: _ when v <> "" -> { s with OFFSET = float32 v }
            | "BPMS", bs :: _ ->
                match run parse_pairs (bs.ReplaceLineEndings("").Trim ',') with
                | Success(result, _, _) ->
                    { s with
                        BPMS =
                            result
                            |> List.map (fun (a, b) -> (float32 a * 1.0f<beat>, float32 b * 1.0f<beat / minute>))
                    }
                | Failure(error, _, _) -> failwith error
            | "STOPS", ss :: _ ->
                match run parse_pairs (ss.ReplaceLineEndings("").Trim ',') with
                | Success(result, _, _) ->
                    { s with
                        STOPS = result |> List.map (fun (a, b) -> (float32 a * 1.0f<beat>, float32 b))
                    }
                | Failure(error, _, _) -> s
            | "SAMPLESTART", v :: _ when v <> "" -> { s with SAMPLESTART = float32 v }
            | "SAMPLELENGTH", v :: _ when v <> "" -> { s with SAMPLELENGTH = float32 v }
            | "DISPLAYBPM", "*" :: _ ->
                { s with
                    DISPLAYBPM = (0.0f<beat / minute>, 999.0f<beat / minute>)
                }
            | "DISPLAYBPM", v1 :: v2 :: _ when v2 <> "" ->
                { s with
                    DISPLAYBPM = (float32 v1 * 1.0f<beat / minute>, float32 v2 * 1.0f<beat / minute>)
                }
            | "DISPLAYBPM", v :: _ when v <> "" ->
                { s with
                    DISPLAYBPM = float32 v |> fun x -> (x * 1.0f<beat / minute>, x * 1.0f<beat / minute>)
                }
            | "SELECTABLE", "YES" :: _ -> { s with SELECTABLE = true }
            | "SELECTABLE", "NO" :: _ -> { s with SELECTABLE = false }
            | "NOTES", steps_type :: author :: difficulty_type :: foot_meter :: groove :: notes :: _ ->
                match run parse_measures notes with
                | Success(parsedNotes, _, _) ->
                    { s with
                        Charts =
                            {
                                NOTES = parsedNotes
                                CHARTNAME = difficulty_type + " " + foot_meter
                                STEPSTYPE = parse_chart_type steps_type
                                DESCRIPTION = ""
                                CHARTSTYLE = ""
                                DIFFICULTY =
                                    match StepManiaDifficultyType.TryParse(difficulty_type, true) with
                                    | true, d -> d
                                    | false, _ -> StepManiaDifficultyType.Beginner
                                METER = foot_meter
                                CREDIT = author
                            }
                            :: s.Charts
                    }
                | Failure(error_message, _, _) -> failwith error_message
            | _ -> s

        List.fold f StepManiaFile.Default header

    let private parse_stepmania_file = parse_header |>> read_stepmania_data

    let parse_file path : Result<StepManiaFile, string> =
        try
            match runParserOnFile parse_stepmania_file () path System.Text.Encoding.UTF8 with
            | Success(result, _, _) -> Result.Ok result
            | Failure(error_message, _, _) -> Result.Error error_message
        with
        | :? IOException as exn -> Result.Error exn.Message

type StepMania() =
    static member FromFile(path: string) : Result<StepManiaFile, string> = StepmaniaParser.parse_file path