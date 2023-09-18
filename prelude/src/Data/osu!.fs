namespace Prelude.Data

open System
open System.IO

module ``osu!`` =
    
    let private read_byte (br: BinaryReader) = br.ReadByte()
    let private read_short (br: BinaryReader) = br.ReadInt16()
    let private read_int (br: BinaryReader) = br.ReadInt32()
    let private read_long (br: BinaryReader) = br.ReadInt64()
    let private read_leb128 (br: BinaryReader) : bigint = failwith "nyi"
    let private read_single (br: BinaryReader) = br.ReadSingle()
    let private read_double (br: BinaryReader) = br.ReadDouble()
    let private read_bool (br: BinaryReader) = br.ReadByte() <> 0x00uy
    let private read_string (br: BinaryReader) =
        let b = br.ReadByte()
        if b = 0x00uy then ""
        elif b = 0x0buy then br.ReadString()
        else failwith "Unknown byte while reading string"

    let private read_int_double_pair (br: BinaryReader) =
        assert(br.ReadByte() = 0x08uy)
        let int = read_int br
        assert(br.ReadByte() = 0x0Duy)
        let double = read_double br
        int, double

    let private read_timing_point (br: BinaryReader) = br.ReadBytes(17) |> ignore

    let timestamp_0001_to_1601 (l: int64) = DateTime(l).ToFileTimeUtc()

    // All assuming your database version is 2015+ because the year is currently 2023
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
            TimingPoints: unit array // data is discarded cause i dont care
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
                StandardModeStarRatings = Array.init (read_int br) (fun i -> read_int_double_pair br)
                TaikoModeStarRatings = Array.init (read_int br) (fun i -> read_int_double_pair br)
                CatchModeStarRatings = Array.init (read_int br) (fun i -> read_int_double_pair br)
                ManiaModeStarRatings = Array.init (read_int br) (fun i -> read_int_double_pair br)
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

    type OsuDatabase =
        {
            Version: int
            FolderCount: int
            AccountUnlocked: bool
            AccountUnlockDate: DateTime
            PlayerName: string
            Beatmaps: OsuDatabase_Beatmap array
            UserPermissions: int
        }
        static member Read (br: BinaryReader) =
            let version = read_int br
            {
                Version = version
                FolderCount = read_int br
                AccountUnlocked = read_bool br
                AccountUnlockDate = read_long br |> DateTime
                PlayerName = read_string br
                Beatmaps = Array.init (read_int br) (fun i -> OsuDatabase_Beatmap.Read version br)
                UserPermissions = read_int br
            }

    type ScoreDatabase_Score =
        {
            Mode: byte
            Version: int
            BeatmapHash: string
            Player: string
            ReplayHash: string
            Count300: int16
            Count100: int16
            Count50: int16
            CountGeki: int16 // = 320s in mania
            CountKatu: int16 // = 200s in mania
            CountMiss: int16
            Score: int
            MaxCombo: int16
            PerfectCombo: bool
            ModsUsed: int
            LifeBarGraph: string
            Timestamp: int64
            CompressedReplayBytes: byte array option
            OnlineScoreID: int64
            // if you have target practice scores in your database, suck it up
        }
        static member Read (br: BinaryReader) =
            {
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
                ModsUsed = read_int br
                LifeBarGraph = read_string br
                Timestamp = read_long br |> timestamp_0001_to_1601
                CompressedReplayBytes =
                    let length = read_int br
                    if length < 0 then None else Some <| br.ReadBytes length
                OnlineScoreID = read_long br
            }

    type ScoreDatabase_Beatmap =
        {
            Hash: string
            Scores: ScoreDatabase_Score array
        }
        static member Read (br: BinaryReader) =
            {
                Hash = read_string br
                Scores = Array.init (read_int br) (fun i -> ScoreDatabase_Score.Read br)
            }

    type ScoreDatabase =
        {
            Version: int
            Beatmaps: ScoreDatabase_Beatmap array
        }
        static member Read (br: BinaryReader) =
            {
                Version = read_int br
                Beatmaps = Array.init (read_int br) (fun i -> ScoreDatabase_Beatmap.Read br)
            }
