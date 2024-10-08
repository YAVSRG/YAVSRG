﻿namespace Prelude.Charts.Formats.StepMania

open System.IO
open FParsec
open Prelude

//https://github.com/stepmania/stepmania/wiki/sm
//https://github.com/stepmania/stepmania/wiki/ssc
(*
    Parsing tools for reading overall .sm structure
*)

type Header = (string * string list) list

type StepManiaDifficultyType =
    | Beginner = 0
    | Easy = 1
    | Medium = 2
    | Hard = 3
    | Challenge = 4
    | Edit = 5

type StepManiaChartType =
    | Dance_Single
    | Dance_Double
    | Dance_Solo
    | Dance_Couple
    | Dance_Threepanel
    | Dance_Routine
    | Pump_Single
    | Pump_Halfdouble
    | Pump_Double
    | Pump_Couple
    | Pump_Routine
    | Kb7_Single
    | UNKNOWN
    member this.Keycount =
        match this with
        | Dance_Threepanel -> 3
        | Dance_Single
        | UNKNOWN -> 4
        | Pump_Single -> 5
        | Dance_Solo
        | Pump_Halfdouble -> 6
        | Kb7_Single -> 7
        | Dance_Couple
        | Dance_Double
        | Dance_Routine -> 8
        | Pump_Couple
        | Pump_Double
        | Pump_Routine -> 10

    override this.ToString() = sprintf "%iK" this.Keycount

type StepManiaNote =
    | Nothing = '0'
    | Normal = '1'
    | Hold_Head = '2'
    | Hold_Tail = '3'
    | Roll_Head = '4'
    | Mine = 'M'
    | Lift = 'L'
    | Fake = 'F'

type Measure = string list

type StepManiaChart =
    {
        NOTES: Measure list
        CHARTNAME: string
        STEPSTYPE: StepManiaChartType
        DESCRIPTION: string
        CHARTSTYLE: string
        DIFFICULTY: StepManiaDifficultyType
        METER: string
        CREDIT: string
    }
    
(*
    Some common tags have been omitted due to having no relevance or poor documentation
*)

type StepManiaFile =
    {
        TITLE: string
        SUBTITLE: string
        ARTIST: string
        TITLETRANSLIT: string
        SUBTITLETRANSLIT: string
        ARTISTTRANSLIT: string
        GENRE: string
        CREDIT: string
        BANNER: string
        BACKGROUND: string
        CDTITLE: string
        MUSIC: string
        OFFSET: float32
        BPMS: (float32<beat> * float32<beat / minute>) list
        STOPS: (float32<beat> * float32) list
        SAMPLESTART: float32
        SAMPLELENGTH: float32
        DISPLAYBPM: float32<beat / minute> * float32<beat / minute>
        SELECTABLE: bool
        //The following tags are SSC features
        VERSION: string
        ORIGIN: string
        WARPS: (float32<beat> * float32) list
        DELAYS: (float32<beat> * float32) list
        TIMESIGNATURES: (float32<beat> * float32<beat>) list
        Charts: StepManiaChart list
    }
    static member Default =
        {
            TITLE = "Unknown Chart"
            SUBTITLE = ""
            ARTIST = ""
            TITLETRANSLIT = ""
            SUBTITLETRANSLIT = ""
            ARTISTTRANSLIT = ""
            GENRE = ""
            CREDIT = ""
            BANNER = ""
            BACKGROUND = ""
            CDTITLE = ""
            MUSIC = ""
            OFFSET = 0.0f
            BPMS = [ (0.0f<beat>, 120.0f<beat / minute>) ]
            STOPS = []
            SAMPLESTART = 0.0f
            SAMPLELENGTH = 1.0f
            DISPLAYBPM = (120.0f<beat / minute>, 120.0f<beat / minute>)
            SELECTABLE = true
            VERSION = ""
            ORIGIN = ""
            WARPS = []
            DELAYS = []
            TIMESIGNATURES = []
            Charts = []
        }

module StepmaniaParser =

    let private comment = optional (skipString "//" >>. skipRestOfLine true >>. spaces)

    let private text = 
        let normal_char = noneOf ":;"
        let escaped_char = skipChar '\\' >>. anyChar
        manyChars (notFollowedBy (newline .>> skipChar '#') >>. (escaped_char <|> normal_char)) |>> fun s -> s.Trim()

    let private parse_key_value =
        comment >>. (skipChar '#') >>. text .>> skipChar ':'
        .>>. (sepBy text (skipChar ':'))
        .>> (optional (skipChar ';') >>. skipRestOfLine true)
        .>> spaces

    let private parse_header: Parser<Header, unit> = many parse_key_value .>> eof
    
    let private parse_note_row = many ((many1Chars (anyOf "01234MLF")) .>> spaces)

    let private parse_measures =
        (optional (spaces >>. comment .>> spaces))
        >>. (sepBy parse_note_row (pchar ',' .>> spaces .>> (optional (comment .>> spaces))))
        .>> eof

    let private parse_pairs =
        (sepBy (pfloat .>> pchar '=' .>>. pfloat .>> spaces) (pchar ',') .>> eof)

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
            | "TITLE", [ t ] -> { s with TITLE = t }
            | "SUBTITLE", [ t ] -> { s with SUBTITLE = t }
            | "ARTIST", [ t ] -> { s with ARTIST = t }
            | "TITLETRANSLIT", [ t ] -> { s with TITLETRANSLIT = t }
            | "SUBTITLETRANSLIT", [ t ] -> { s with SUBTITLETRANSLIT = t }
            | "ARTISTTRANSLIT", [ t ] -> { s with ARTISTTRANSLIT = t }
            | "GENRE", [ t ] -> { s with GENRE = t }
            | "CREDIT", [ t ] -> { s with CREDIT = t }
            | "BANNER", [ t ] -> { s with BANNER = t }
            | "BACKGROUND", [ t ] -> { s with BACKGROUND = t }
            | "CDTITLE", [ t ] -> { s with CDTITLE = t }
            | "MUSIC", [ t ] -> { s with MUSIC = t }
            | "OFFSET", [ v ] -> { s with OFFSET = float32 v }
            | "BPMS", [ bs ] ->
                match run parse_pairs bs with
                | Success(result, _, _) ->
                    { s with
                        BPMS =
                            result
                            |> List.map (fun (a, b) -> (float32 a * 1.0f<beat>, float32 b * 1.0f<beat / minute>))
                    }
                | Failure(errorMsg, _, _) -> failwith errorMsg
            | "STOPS", [ ss ] ->
                match run parse_pairs ss with
                | Success(result, _, _) ->
                    { s with
                        STOPS = result |> List.map (fun (a, b) -> (float32 a * 1.0f<beat>, float32 b))
                    }
                | Failure(errorMsg, _, _) -> failwith errorMsg
            | "SAMPLESTART", [ v ] -> { s with SAMPLESTART = float32 v }
            | "SAMPLELENGTH", [ v ] -> { s with SAMPLELENGTH = float32 v }
            | "DISPLAYBPM", [ "*" ] ->
                { s with
                    DISPLAYBPM = (0.0f<beat / minute>, 999.0f<beat / minute>)
                }
            | "DISPLAYBPM", [ v ] ->
                { s with
                    DISPLAYBPM = float32 v |> fun x -> (x * 1.0f<beat / minute>, x * 1.0f<beat / minute>)
                }
            | "DISPLAYBPM", [ v1; v2 ] ->
                { s with
                    DISPLAYBPM = (float32 v1 * 1.0f<beat / minute>, float32 v2 * 1.0f<beat / minute>)
                }
            | "SELECTABLE", [ "YES" ] -> { s with SELECTABLE = true }
            | "SELECTABLE", [ "NO" ] -> { s with SELECTABLE = false }
            | "NOTES", [ chartType; author; difficultyType; footMeter; groove; noteData ] ->
                match run parse_measures noteData with
                | Success(parsedNotes, _, _) ->
                    { s with
                        Charts =
                            {
                                NOTES = parsedNotes
                                CHARTNAME = difficultyType + " " + footMeter
                                STEPSTYPE = parse_chart_type chartType
                                DESCRIPTION = ""
                                CHARTSTYLE = ""
                                DIFFICULTY = StepManiaDifficultyType.Parse(difficultyType, true)
                                METER = footMeter
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