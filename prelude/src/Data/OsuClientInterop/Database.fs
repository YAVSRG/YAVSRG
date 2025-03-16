namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Mods

type Mods =
    | None = 0
    | NoFail = 1
    | Easy = 2
    | TouchDevice = 4
    | Hidden = 8
    | HardRock = 16
    | SuddenDeath = 32
    | DoubleTime = 64
    | Relax = 128
    | HalfTime = 256
    | Nightcore = 512 // always used with DT
    | Flashlight = 1024
    | Autoplay = 2048
    | SpunOut = 4096
    | Autopilot = 8192
    | Perfect = 16384
    | Key4 = 32768
    | Key5 = 65536
    | Key6 = 131072
    | Key7 = 262144
    | Key8 = 524288
    | FadeIn = 1048576
    | Random = 2097152
    | Cinema = 4194304
    | TargetPractice = 8388608
    | Key9 = 16777216
    | Coop = 33554432
    | Key1 = 67108864
    | Key3 = 134217728
    | Key2 = 268435456
    | ScoreV2 = 536870912
    | Mirror = 1073741824

module Mods =

    let interlude_recognised_mods =
        Mods.NoFail
        ||| Mods.Easy
        ||| Mods.Hidden
        ||| Mods.HardRock
        ||| Mods.SuddenDeath
        ||| Mods.DoubleTime
        ||| Mods.HalfTime
        ||| Mods.Nightcore
        ||| Mods.Flashlight
        ||| Mods.Perfect
        ||| Mods.FadeIn
        ||| Mods.ScoreV2
        ||| Mods.Mirror

    let to_interlude_rate_and_mods (mods: Mods) : (Rate * ModState) option =
        if mods &&& interlude_recognised_mods <> mods then
            None
        else

            let rate =
                if mods &&& Mods.DoubleTime = Mods.DoubleTime then 1.5f<rate>
                elif mods &&& Mods.HalfTime = Mods.HalfTime then 0.75f<rate>
                else 1.0f<rate>

            let mod_state =
                seq {
                    if mods &&& Mods.Mirror = Mods.Mirror then
                        yield ("mirror", 0)
                }
                |> Map.ofSeq

            Some(rate, mod_state)

[<AutoOpen>]
module internal OsuDbHelpers =

    let read_byte (br: BinaryReader) : byte = br.ReadByte()
    let read_short (br: BinaryReader) : int16 = br.ReadInt16()
    let read_int (br: BinaryReader) : int = br.ReadInt32()
    let read_long (br: BinaryReader) : int64 = br.ReadInt64()
    let read_single (br: BinaryReader) : float32 = br.ReadSingle()
    let read_double (br: BinaryReader) : float = br.ReadDouble()
    let read_bool (br: BinaryReader) : bool = br.ReadByte() <> 0x00uy

    let read_string (br: BinaryReader) : string =
        let b = br.ReadByte()

        if b = 0x00uy then ""
        elif b = 0x0buy then br.ReadString()
        else failwith "Unknown byte while reading string"

    let read_int_double_pair (br: BinaryReader) : int * float =
        if br.ReadByte() <> 0x08uy then
            failwith "Got unexpected byte at start of int-double pair"

        let int = read_int br

        let double =
            match br.ReadByte() with
            | 0x0Duy -> read_double br
            | 0x0Cuy -> read_single br |> float
            | other -> failwithf "Got unexpected byte %X for float part of int-double pair" other

        int, double

    let read_star_ratings (br: BinaryReader) : (int * float) array =
        let count = read_int br

        if count < 0 then
            failwith "Something has gone wrong reading star ratings (meaning misaligned bytes earlier on)"

        Array.init count (fun i -> read_int_double_pair br)

    let write_string (bw: BinaryWriter) (s: string) =
        if s = "" then
            bw.Write(0x00uy)
        else
            bw.Write(0x0buy)
            bw.Write s

    let read_timing_point (br: BinaryReader) = br.ReadBytes(17) |> ignore

    let timestamp_0001_to_1601 (l: int64) = DateTime(l).ToFileTimeUtc()

// All assuming your database version is 2015+ because the year is currently 2024
type OsuDatabase_Beatmap =
    {
        Size: int option
        Artist: string
        ArtistUnicode: string
        Title: string
        TitleUnicode: string
        Creator: string
        Difficulty: string
        AudioFile: string
        Hash: string
        Filename: string
        Status: byte
        Hitcircles: int16
        Sliders: int16
        Spinners: int16
        LastModified: int64
        ApproachRate: float32
        CircleSize: float32
        HPDrain: float32
        OverallDifficulty: float32
        SliderVelocity: float
        StandardModeStarRatings: (int * float) array
        TaikoModeStarRatings: (int * float) array
        CatchModeStarRatings: (int * float) array
        ManiaModeStarRatings: (int * float) array
        DrainTimeSeconds: int
        TotalTimeMilliseconds: int
        PreviewTimeMilliseconds: int
        TimingPoints: unit array // Data is discarded because it's not very useful
        DifficultyID: int
        BeatmapID: int
        ThreadID: int
        StandardModeGrade: byte
        TaikoModeGrade: byte
        CatchModeGrade: byte
        ManiaModeGrade: byte
        LocalOffset: int16
        StackLeniency: float32
        Mode: byte
        Source: string
        Tags: string
        OnlineOffset: int16
        TitleFont: string
        Unplayed: bool
        LastPlayed: int64
        IsOsz2: bool
        FolderName: string
        LastRepositoryCheck: int64
        IgnoreBeatmapSound: bool
        IgnoreBeatmapSkin: bool
        DisableStoryboard: bool
        DisableVideo: bool
        VisualOverride: bool
        LastModified2: int
        ManiaScrollSpeed: byte
    }
    static member Read (db_version: int) (br: BinaryReader) =
        try
            {
                Size = if db_version < 20191106 then read_int br |> Some else None
                Artist = read_string br
                ArtistUnicode = read_string br
                Title = read_string br
                TitleUnicode = read_string br
                Creator = read_string br
                Difficulty = read_string br
                AudioFile = read_string br
                Hash = read_string br
                Filename = read_string br
                Status = read_byte br
                Hitcircles = read_short br
                Sliders = read_short br
                Spinners = read_short br
                LastModified = read_long br
                ApproachRate = read_single br
                CircleSize = read_single br
                HPDrain = read_single br
                OverallDifficulty = read_single br
                SliderVelocity = read_double br
                StandardModeStarRatings = read_star_ratings br
                TaikoModeStarRatings = read_star_ratings br
                CatchModeStarRatings = read_star_ratings br
                ManiaModeStarRatings = read_star_ratings br
                DrainTimeSeconds = read_int br
                TotalTimeMilliseconds = read_int br
                PreviewTimeMilliseconds = read_int br
                TimingPoints = Array.init (read_int br) (fun i -> read_timing_point br)
                DifficultyID = read_int br
                BeatmapID = read_int br
                ThreadID = read_int br
                StandardModeGrade = read_byte br
                TaikoModeGrade = read_byte br
                CatchModeGrade = read_byte br
                ManiaModeGrade = read_byte br
                LocalOffset = read_short br
                StackLeniency = read_single br
                Mode = read_byte br
                Source = read_string br
                Tags = read_string br
                OnlineOffset = read_short br
                TitleFont = read_string br
                Unplayed = read_bool br
                LastPlayed = read_long br
                IsOsz2 = read_bool br
                FolderName = read_string br
                LastRepositoryCheck = read_long br
                IgnoreBeatmapSound = read_bool br
                IgnoreBeatmapSkin = read_bool br
                DisableStoryboard = read_bool br
                DisableVideo = read_bool br
                VisualOverride = read_bool br
                LastModified2 = read_int br
                ManiaScrollSpeed = read_byte br
            }
        with err ->
            Logging.Error "Exception occured at position %i" br.BaseStream.Position
            reraise ()

type OsuDatabase =
    {
        Version: int
        FolderCount: int
        AccountUnlocked: bool
        AccountUnlockDate: int64
        PlayerName: string
        Beatmaps: OsuDatabase_Beatmap array
        UserPermissions: int
    }
    static member Read(br: BinaryReader) =
        let version = read_int br

        {
            Version = version
            FolderCount = read_int br
            AccountUnlocked = read_bool br
            AccountUnlockDate = read_long br
            PlayerName = read_string br
            Beatmaps =
                let count = read_int br
                Logging.Info "osu! Database header says there are %i beatmaps to read" count
                Array.init count (fun i -> OsuDatabase_Beatmap.Read version br)
            UserPermissions = read_int br
        }

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

    static member Read(file_path: string option, br: BinaryReader) =
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
                if length < 0 then None else Some <| br.ReadBytes length
            OnlineScoreID = read_long br
        }

    static member TryReadFile(file_path: string) =
        try
            use file = File.OpenRead file_path
            use br = new BinaryReader(file)
            Some(OsuScoreDatabase_Score.Read(Some file_path, br))
        with err ->
            Logging.Error "Error loading replay file '%s': %O" file_path err
            None

    member this.Write(bw: BinaryWriter) =
        bw.Write this.Mode
        bw.Write this.Version
        write_string bw this.BeatmapHash
        write_string bw this.Player
        write_string bw this.ReplayHash
        bw.Write this.Count300
        bw.Write this.Count100
        bw.Write this.Count50
        bw.Write this.CountGeki
        bw.Write this.CountKatu
        bw.Write this.CountMiss
        bw.Write this.Score
        bw.Write this.MaxCombo
        bw.Write this.PerfectCombo
        bw.Write(int this.ModsUsed)
        write_string bw this.LifeBarGraph
        bw.Write(DateTime.FromFileTimeUtc(this.Timestamp).Ticks)

        match this.CompressedReplayBytes with
        | Some b ->
            bw.Write b.Length
            bw.Write b
        | None -> bw.Write -1

        bw.Write this.OnlineScoreID

type OsuScoreDatabase_Beatmap =
    {
        Hash: string
        Scores: OsuScoreDatabase_Score array
    }
    static member Read(br: BinaryReader) =
        {
            Hash = read_string br
            Scores = Array.init (read_int br) (fun i -> OsuScoreDatabase_Score.Read(None, br))
        }

type OsuScoreDatabase =
    {
        Version: int
        Beatmaps: OsuScoreDatabase_Beatmap array
    }
    static member Read(br: BinaryReader) =
        {
            Version = read_int br
            Beatmaps = Array.init (read_int br) (fun i -> OsuScoreDatabase_Beatmap.Read br)
        }