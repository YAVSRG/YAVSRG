namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open Percyqaz.Common

type OsuScoreDatabase_Score =
    {
        FilePath: string option
        Mode: byte
        Version: int
        BeatmapHash: string
        Player: string
        ReplayHash: string
        Count300: int16
        Count100: int16
        Count50: int16
        /// = 320s in mania
        CountGeki: int16
        /// = 200s in mania
        CountKatu: int16
        CountMiss: int16
        Score: int
        MaxCombo: int16
        PerfectCombo: bool
        ModsUsed: Mods
        LifeBarGraph: string
        Timestamp: int64
        CompressedReplayBytes: byte array option
        OnlineScoreID: int64
    // Incomplete / may crash if you have target practice scores in your database
    }

    member this.IsReplayFile = this.FilePath.IsSome

    static member Read(file_path: string option, br: BinaryReader) : OsuScoreDatabase_Score =
        {
            FilePath = file_path
            Mode = read_byte br
            Version = read_int br
            BeatmapHash = read_string br
            Player = read_string br
            ReplayHash = read_string br
            Count300 = read_short br
            Count100 = read_short br
            Count50 = read_short br
            CountGeki = read_short br
            CountKatu = read_short br
            CountMiss = read_short br
            Score = read_int br
            MaxCombo = read_short br
            PerfectCombo = read_bool br
            ModsUsed = read_int br |> enum
            LifeBarGraph = read_string br
            Timestamp = read_long br |> timestamp_0001_to_1601
            CompressedReplayBytes =
                let length = read_int br
                if length < 0 then None else Some(br.ReadBytes(length))
            OnlineScoreID = read_long br
        }

    static member TryReadFile(file_path: string) : OsuScoreDatabase_Score option =
        try
            use file = File.OpenRead(file_path)
            use br = new BinaryReader(file)
            Some(OsuScoreDatabase_Score.Read(Some file_path, br))
        with err ->
            Logging.Error "Error loading replay file '%s': %O" file_path err
            None

    member this.Write(bw: BinaryWriter) : unit =
        bw.Write(this.Mode)
        bw.Write(this.Version)
        write_string bw this.BeatmapHash
        write_string bw this.Player
        write_string bw this.ReplayHash
        bw.Write(this.Count300)
        bw.Write(this.Count100)
        bw.Write(this.Count50)
        bw.Write(this.CountGeki)
        bw.Write(this.CountKatu)
        bw.Write(this.CountMiss)
        bw.Write(this.Score)
        bw.Write(this.MaxCombo)
        bw.Write(this.PerfectCombo)
        bw.Write(int this.ModsUsed)
        write_string bw this.LifeBarGraph
        bw.Write(DateTime.FromFileTimeUtc(this.Timestamp).Ticks)

        match this.CompressedReplayBytes with
        | Some b ->
            bw.Write(b.Length)
            bw.Write(b)
        | None -> bw.Write(-1)

        bw.Write(this.OnlineScoreID)

type OsuScoreDatabase_Beatmap =
    {
        Hash: string
        Scores: OsuScoreDatabase_Score array
    }

    static member Read(br: BinaryReader) : OsuScoreDatabase_Beatmap =
        { Hash = read_string br; Scores = Array.init (read_int br) (fun _ -> OsuScoreDatabase_Score.Read(None, br)) }

type OsuScoreDatabase =
    {
        Version: int
        Beatmaps: OsuScoreDatabase_Beatmap array
    }

    static member Read(br: BinaryReader) : OsuScoreDatabase =
        { Version = read_int br; Beatmaps = Array.init (read_int br) (fun _ -> OsuScoreDatabase_Beatmap.Read(br)) }
