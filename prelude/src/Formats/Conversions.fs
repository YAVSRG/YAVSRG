namespace Prelude.Formats

open System
open System.IO

type ConversionOptions =
    {
        MoveAssets: bool
        EtternaPackName: string option
        ChangedAfter: DateTime option
        PackName: string
    }
    static member Default =
        {
            MoveAssets = false
            EtternaPackName = None
            ChangedAfter = None
            PackName = "Singles"
        }

type ConversionAction =
    {
        Config: ConversionOptions
        Source: string
    }

type SkippedConversion = string * string
type ConversionResult =
    {
        ConvertedCharts: int
        SkippedCharts: SkippedConversion list
    }
    static member Empty = { ConvertedCharts = 0; SkippedCharts = [] }
    static member Combine (res1: ConversionResult) (res2: ConversionResult) =
        {
            ConvertedCharts = res1.ConvertedCharts + res2.ConvertedCharts
            SkippedCharts = res1.SkippedCharts @ res2.SkippedCharts
        }

exception ConversionSkipException of msg: string

(*
    Overall utilities to dynamically load different chart files and convert to Interlude format
*)

[<AutoOpen>]
module Utilities =

    let inline skip_conversion (msg: string) = raise (ConversionSkipException msg)

    let (|ChartFile|_|) (path: string) =
        let s = Path.GetExtension(path).ToLower()
        match s with
        | ".sm"
        | ".qua"
        | ".osu" -> Some s
        | _ -> None

    let (|ChartArchive|_|) (path: string) =
        let s = Path.GetExtension(path).ToLower()
        match s with
        | ".osz"
        | ".qp"
        | ".zip" -> Some s
        | _ -> None

    let (|SongFolder|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateFiles path
            |> Seq.tryPick (fun x ->
                match x with
                | ChartFile s -> Some s
                | _ -> None
            )
        else
            None

    let (|PackFolder|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateDirectories path
            |> Seq.forall (fun x ->
                match x with
                | SongFolder _ -> false
                | _ -> true
            )
            |> fun b -> if b then None else Some()
        else
            None

    let (|FolderOfPacks|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateDirectories path
            |> Seq.forall (fun x ->
                match x with
                | PackFolder -> false
                | _ -> true
            )
            |> fun b -> if b then None else Some()
        else
            None

    let (|FolderOfOszs|_|) (path: string) =
        if Directory.Exists path then
            if
                Directory.EnumerateFiles path
                |> Seq.exists (fun x ->
                    match x with
                    | ChartArchive ".osz" -> true
                    | _ -> false
                )
            then Some()
            else None
        else
            None