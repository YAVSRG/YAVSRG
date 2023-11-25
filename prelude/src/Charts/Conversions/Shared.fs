namespace Prelude.Charts.Conversions

open System
open System.IO

type ConversionOptions =
    {
        MoveAssets: bool
        StepmaniaPackId: int option
        ChangedAfter: DateTime option
        PackName: string
    }
    static member Default =
        {
            MoveAssets = false
            StepmaniaPackId = None
            ChangedAfter = None
            PackName = "Singles"
        }

type ConversionAction =
    {
        Config: ConversionOptions
        Source: string
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
        | ".osu" -> Some s
        | _ -> None

    let (|ChartArchive|_|) (path: string) =
        match Path.GetExtension(path).ToLower() with
        | ".osz"
        | ".zip" -> Some()
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
