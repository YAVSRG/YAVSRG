namespace Prelude.Formats

open System
open System.IO
open Prelude

type ConversionAssetBehaviour =
    | CopyAssetFiles
    | LinkAssetFiles

type ConversionOptions =
    {
        AssetBehaviour: ConversionAssetBehaviour
        EtternaPackName: string option
        ChangedAfter: DateTime option
        PackName: string
    }

    static member EtternaPack(pack_name: string, changed_after: DateTime option, asset_behaviour: ConversionAssetBehaviour) =
        {
            AssetBehaviour = asset_behaviour
            EtternaPackName = Some pack_name
            ChangedAfter = changed_after
            PackName = pack_name
        }

    static member Pack(pack_name: string, changed_after: DateTime option, asset_behaviour: ConversionAssetBehaviour) =
        {
            AssetBehaviour = asset_behaviour
            EtternaPackName = None
            ChangedAfter = changed_after
            PackName = pack_name
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

    let cleaned_sv (sv: TimeArray<float32>) : TimeArray<float32> =
        seq {
            let deduped =
                sv
                |> Array.rev
                |> Array.distinctBy _.Time
                |> Array.rev
            let mutable previous_value = 1.0f
            for s in deduped do
                if abs (s.Data - previous_value) > 0.005f then
                    yield s
                    previous_value <- s.Data
        }
        |> Array.ofSeq

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