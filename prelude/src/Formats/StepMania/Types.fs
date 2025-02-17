namespace Prelude.Formats.StepMania

open Prelude

//https://github.com/stepmania/stepmania/wiki/sm
//https://github.com/stepmania/stepmania/wiki/ssc

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
        | Dance_Single -> 4
        | UNKNOWN -> -1
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

    override this.ToString() =
        match this with
        | UNKNOWN -> ""
        | _ -> sprintf "%iK " this.Keycount

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