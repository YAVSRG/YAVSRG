namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open Percyqaz.Common

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

    static member Read(db_version: int, br: BinaryReader) : OsuDatabase_Beatmap =
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
                TimingPoints = Array.init (read_int br) (fun _ -> read_timing_point br)
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
            reraise()

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

    static member Read(br: BinaryReader) : OsuDatabase =
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
                Array.init count (fun _ -> OsuDatabase_Beatmap.Read(version, br))
            UserPermissions = read_int br
        }
