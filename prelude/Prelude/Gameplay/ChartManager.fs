module Prelude.Charts.ChartManager

open Newtonsoft.Json
open System
open System.IO
open System.Collections.Generic
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.ChartConversions
open Prelude.Gameplay.Difficulty

(*
    Caching of charts
*)

type CachedChart = {
    File: string
    SourcePath: string
    Title: string
    Artist: string
    Creator: string
    Pack: string
    Hash: string
    Keys: int
    Length: float
    BPM: (float * float)
    DiffName: string
    Physical: float
    Technical: float

    [<JsonIgnore>]
    Collection: string
    [<JsonIgnore>]
    CollectionIndex: int }

let cacheChart (chart : Chart) : CachedChart =
    let endTime =
        if chart.Notes.Count = 0 then 0.0 else
            chart.Notes.GetPointAt infinity |> offsetOf
    let rating = RatingReport(chart.Notes, 1.0, Layout.Spread, chart.Keys)
    {
    File = chart.Header.File
    SourcePath = chart.Header.SourcePath
    Title = chart.Header.Title
    Artist = chart.Header.Artist
    Creator = chart.Header.Creator
    Pack = chart.Header.SourcePack
    Hash = calculateHash chart
    Keys = chart.Keys
    Length = if endTime = 0.0 then 0.0 else endTime - (offsetOf chart.Notes.First)
    BPM = minMaxBPM (chart.BPM.Enumerate |> List.ofSeq) endTime
    DiffName = chart.Header.DiffName
    Physical = rating.Physical
    Technical = rating.Technical
    Collection = ""
    CollectionIndex = 0 }

(*this stuff should stay in Interlude as ColorBy uses aspects that belong in Interlude

let private formatFirstChar (s : string) =
    if s.Length > 0 && System.Char.IsLetterOrDigit s.[0] then s.[0].ToString().ToUpper() else "?"
let private formatNum (n : float) =
    let i = int (n / 2.0) * 2
    i.ToString().PadLeft(2, '0') + " - " + (i + 2).ToString().PadLeft(2, '0')

let GroupBy : IDictionary<string, CachedChart -> string> =
    dict (seq [
        ("Physical", fun c -> c.Physical |> formatNum);
        ("Technical", fun c -> c.Technical |> formatNum);
        ("Creator", fun c -> c.Creator |> formatFirstChar);
        ("Artist", fun c -> c.Artist |> formatFirstChar);
        ("Pack", fun c -> c.Pack);
        ("Title", fun c -> c.Title |> formatFirstChar);
        ("Keymode", fun c -> c.Keys.ToString() + "K");
        ("Collection", fun c -> "")
    ])
*)

let osuSongFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "osu!", "Songs")
let smPackFolder = Path.Combine(Path.GetPathRoot(Environment.CurrentDirectory), "Games", "Stepmania 5", "Songs")
let etternaPackFolder = Path.Combine(Path.GetPathRoot(Environment.CurrentDirectory), "Games", "Etterna", "Songs")
type Cache() =
    let charts = new Dictionary<string, CachedChart>()
    
    member this.CacheChart (c: Chart) = lock(this) (fun () -> charts.[c.FileIdentifier] <- cacheChart c)

    member this.LoadChart (cc : CachedChart) : Chart option =  
        let id = Path.Combine(cc.SourcePath, cc.File)
        try
            let c = id |> loadChartFile
            this.CacheChart c
            Some c
        with
        | err -> Logging.Error ("Could not load chart from " + id) (err.ToString()); None