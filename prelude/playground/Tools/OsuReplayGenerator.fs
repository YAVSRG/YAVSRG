module OsuReplayGenerator

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Formats.Osu
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Data.OsuClientInterop
open Prelude.Data
open Prelude.Data.Library
open Prelude.Tests.Rulesets

let mutable recent_beatmap_hash = ""

let generate_scenario (notes: TimeArray<NoteRow>) (replay: ReplayData) (od: float32) (mods: Mods) =

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
            Origins = Set.empty

            Keys = 4
            Length = 0.0f<ms>
            BPM = 0
            DateAdded = 0L
            Rating = 0.0f
            Patterns = Unchecked.defaultof<_>
        }

    match Exports.create_osz { OD = float od; HP = 8.0 } chart chart_meta "." with
    | Error exn -> failwithf "Couldn't export .osz: %s" exn.Message
    | Ok (beatmap, file_name) ->

    // requires osu! to be open already

    Diagnostics.Process
        .Start(new Diagnostics.ProcessStartInfo(file_name, UseShellExecute = true))
        .WaitForExit()
    |> ignore

    let beatmap_hash = Beatmap.Hash beatmap

    if recent_beatmap_hash <> beatmap_hash then
        Threading.Thread.Sleep(1000)
        recent_beatmap_hash <- beatmap_hash

    let osu_replay = OsuReplay.encode replay notes.[0].Time mods beatmap_hash
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
        match! WebServices.download_json_async<GosuMemoryData> "http://localhost:24050/json" with
        | WebResult.Ok d ->
            Logging.Info "Experiment results:\n%O" d
        | WebResult.HttpError c -> printfn "Error getting GosuMemory data or it isn't running (HTTP ERROR %i)" c
        | WebResult.Exception err -> printfn "Error getting GosuMemory data or it isn't running\n%O" err
    }

let HANDS_FREE_AUTOMATION = true

let run_experiment () =

    // have osu! and GosuMemory running

    let od = 8.0f

    let perfect = floor_uom (OsuMania.perfect_window od) * 1.2f * 1.0f<rate> |> floor_uom
    let great = floor_uom (OsuMania.great_window od) * 1.1f * 1.0f<rate> |> floor_uom
    let good = floor_uom (OsuMania.good_window od) * 1.0f<rate> |> floor_uom
    let ok = floor_uom (OsuMania.ok_window od) * 1.0f<rate> |> floor_uom
    let meh = floor_uom (OsuMania.meh_window od) * 1.0f<rate> |> floor_uom
    let miss = floor_uom (OsuMania.miss_window od) * 1.0f<rate> |> floor_uom

    for head in [104.0f<ms>] do

        for tail in [-miss; -meh; -ok; -good; -great; -perfect; perfect; great; good; ok; meh; miss] do

            let notes =
                ChartBuilder(4)
                    .Hold(0.0f<ms>, 800.0f<ms>)
                    .Build()

            let replay =
                ReplayBuilder()
                    .KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail)
                    .Build()
                    .GetFullReplay()

            Logging.Info "Experiment: LN combination experiment: OD%.1f; Head %.1fms; Tail %.1fms" od head tail

            generate_scenario notes replay od Mods.None

            Threading.Thread.Sleep(2000)
            if HANDS_FREE_AUTOMATION then
                Diagnostics.Process
                    .Start(new Diagnostics.ProcessStartInfo("click_replay_button.ahk", UseShellExecute = true))
                    .WaitForExit()
                Threading.Thread.Sleep(8000)
            else
                Console.ReadKey() |> ignore

            collect_results () |> Async.RunSynchronously