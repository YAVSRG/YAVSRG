module OsuReplayGenerator

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Charts.Formats.osu
open Prelude.Gameplay
open Prelude.Data.OsuClientInterop
open Prelude.Data.Library
open Prelude.Tests.Rulesets

let generate_scenario (notes: TimeArray<NoteRow>) (replay: ReplayData) =

    let chart : Chart = 
        {
            Keys = 4
            Notes = notes
            BPM = [|{ Time = 0.0f<ms>; Data = { Meter = 4<beat>; MsPerBeat = 500.0f<ms / beat> } }|]
            SV = [||]
        }

    let chart_meta : ChartMeta =
        {
            Hash = ""
            Title = "Auto-generated test scenario"
            TitleNative = None
            Artist = "Prelude"
            ArtistNative = None
            DifficultyName = "Auto-generated test scenario"
            Subtitle = None
            Source = Some "YAVSRG"
            Creator = "Percyqaz"
            Tags = ["Yet"; "Another"; "Vertically"; "Scrolling"; "Rhythm"; "Game"]

            Background = AssetPath.Missing
            Audio = AssetPath.Missing
            PreviewTime = 0.0f<ms>

            Packs = Set.empty
            Origin = ChartOrigin.Unknown

            Keys = 4
            Length = 0.0f<ms>
            BPM = 0
            DateAdded = 0L
            Rating = 0.0f
            Patterns = Unchecked.defaultof<_>
        }

    match Exports.create_osz chart chart_meta "." with
    | Error exn -> failwithf "Couldn't export .osz: %s" exn.Message
    | Ok (beatmap, file_name) ->

    // requires osu! to be open already

    Diagnostics.Process
        .Start(new Diagnostics.ProcessStartInfo(file_name, UseShellExecute = true))
        .WaitForExit()
    |> ignore

    let beatmap_hash = Beatmap.Hash beatmap
    let osu_replay = OsuReplay.encode_replay replay notes.[0].Time Mods.None beatmap_hash
    use fs = File.Open("replay.osr", FileMode.Create)
    use bw = new BinaryWriter(fs)
    osu_replay.Write bw
    bw.Flush()
    bw.Close()
    
    Diagnostics.Process
        .Start(new Diagnostics.ProcessStartInfo("replay.osr", UseShellExecute = true))
        .WaitForExit()

[<Json.AutoCodec(false)>]
type GosuMemoryData =
    {
        gameplay: {|
            accuracy: float
            combo: {|
                current: int
                max: int
            |}
            hits: {|
                ``0``: int
                ``50``: int
                ``100``: int
                ``300``: int
                geki: int // 320
                katu: int // 200
                sliderBreaks: int
                hitErrorArray: int array option
            |}
        |}
    }
    override this.ToString() =
        [
            sprintf "%.2f%% %ix/%ix" this.gameplay.accuracy this.gameplay.combo.current this.gameplay.combo.max
            sprintf "%i | %i | %i | %i | %i | %i" this.gameplay.hits.geki this.gameplay.hits.``300`` this.gameplay.hits.katu this.gameplay.hits.``100`` this.gameplay.hits.``50`` this.gameplay.hits.``0``
            this.gameplay.hits.hitErrorArray |> Option.defaultValue [||] |> Seq.map (sprintf "%ims") |> String.concat ", "
        ]
        |> String.concat "\n"

let collect_results () =
    async {
        match! Prelude.Data.WebServices.download_json_async<GosuMemoryData> "http://localhost:24050/json" with
        | Data.WebResult.Ok d ->
            Logging.Info(sprintf "Experiment results:\n%O" d)
        | Data.WebResult.HttpError c -> printfn "Error getting GosuMemory data or it isn't running (HTTP ERROR %i)" c
        | Data.WebResult.Exception err -> printfn "Error getting GosuMemory data or it isn't running\n%O" err
    }

let run_experiment () =

    // have osu! and GosuMemory running

    for hold_time in [1.0f<ms>; 40.0f<ms>; 73.0f<ms>; 103.0f<ms>; 127.0f<ms>; 164.0f<ms>; 165.0f<ms>] do

        let notes = 
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, hold_time)
                .Build()
                .GetFullReplay()

        Logging.Info(sprintf "Experiment: Release after %.0fms" hold_time)

        generate_scenario notes replay
        Console.ReadKey() |> ignore
        collect_results () |> Async.RunSynchronously
