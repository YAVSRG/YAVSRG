module OsuReplayGenerator

open System
open System.IO
open Percyqaz.Common
open Prelude.Data.``osu!``
open Prelude.Data.Library
open SevenZip.Compression

let generate_replay (mods: Mods) (hash: string) : OsuScoreDatabase_Score =
    let inputs : (int * int) seq =
        seq {
            yield -159, -1 // 158 in window // 159 out //158.5 on od10
            yield 130, 500
            yield 1000, 1500
        }
    let input_as_replay_string =
        let mutable previous_time = -1000
        inputs 
        |> Seq.map (fun (press, release) -> 
            let t = previous_time
            previous_time <- release
            sprintf "%i|1|1|0,%i|0|1|0" (press - t) (release - press)
        )
        |> String.concat ","
    let raw =
        sprintf
            "0|256|500|0,0|256|500|0,-1000|0|1|0,%s,-12345|0|0|32767"
            input_as_replay_string
        |> Text.Encoding.UTF8.GetBytes

    use input = new MemoryStream(raw)
    use output = new MemoryStream()

    let encode = new LZMA.Encoder()
    encode.WriteCoderProperties(output)
    output.Write(BitConverter.GetBytes(input.Length), 0, 8)
    encode.Code(input, output, input.Length, -1, null)
    output.Flush()

    {
        Mode = 3uy
        Version = 20220216
        BeatmapHash = hash
        Player = "Percyqaz"
        ReplayHash = hash
        Count300 = 1s
        Count100 = 0s
        Count50 = 0s
        CountGeki = 0s
        CountKatu = 0s
        CountMiss = 0s
        Score = 1000001
        MaxCombo = 727s
        PerfectCombo = true
        ModsUsed = mods
        LifeBarGraph = ""
        Timestamp = DateTime.UtcNow.ToFileTimeUtc()
        CompressedReplayBytes = Some <| output.ToArray()
        OnlineScoreID = 0
    }

let replay_writer () =

    Logging.Info "Reading osu database ..."

    use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "osu!.db") |> File.OpenRead

    use reader = new BinaryReader(file, Text.Encoding.UTF8)
    let main_db = OsuDatabase.Read(reader)

    match
        main_db.Beatmaps
        |> Seq.tryFind (fun b -> b.Difficulty = "thanks for holding")
    with
    | Some osu_db_beatmap ->

        let replay = generate_replay Mods.None osu_db_beatmap.Hash
        use fs = File.Open("replay.osr", FileMode.Create)
        use bw = new BinaryWriter(fs)
        replay.Write bw
        bw.Flush()
        bw.Close()

        Diagnostics.Process
            .Start(new Diagnostics.ProcessStartInfo("replay.osr", UseShellExecute = true))
            .WaitForExit()

    | None -> printfn "didnt find the map"
