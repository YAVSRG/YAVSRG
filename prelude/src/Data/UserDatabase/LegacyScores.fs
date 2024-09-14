namespace Prelude.Data.User

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Gameplay.Mods

[<Json.AutoCodec(true)>]
type LegacyScore =
    {
        time: DateTime
        replay: string
        rate: float32
        selectedMods: ModState
        layout: Layout
        keycount: int
    }
    static member Default =
        {
            time = Unchecked.defaultof<_>
            replay = ""
            rate = 1.0f
            selectedMods = Map.empty
            layout = Layout.Spread
            keycount = 4
        }

    member this.Migrate: Score =
        {
            Timestamp = Timestamp.from_datetime this.time
            Replay = Replay.compressed_string_to_bytes this.replay
            Rate = this.rate
            Mods = this.selectedMods
            IsImported = this.layout = Layout.LeftTwo
            Keys = this.keycount
        }

[<Json.AutoCodec(true)>]
type LegacyBests =
    {
        Lamp: PersonalBests<int>
        Accuracy: PersonalBests<float>
        Grade: PersonalBests<int>
    }
    static member Default = { Lamp = []; Accuracy = []; Grade = [] }

    member this.Migrate: Bests =
        {
            Lamp = this.Lamp
            Accuracy = this.Accuracy
            Grade = this.Grade
        }

[<Json.AutoCodec(false)>]
type LegacyChartSaveData =
    {
        [<Json.Required>]
        mutable Offset: Time
        [<Json.Required>]
        Scores: List<LegacyScore>
        PersonalBests: Dictionary<string, LegacyBests>
        mutable LastPlayed: DateTime
        mutable Comment: string
    }
    static member Default =
        {
            Offset = 0.0f<ms>
            Scores = List<LegacyScore>()
            PersonalBests = Dictionary<string, LegacyBests>()
            LastPlayed = DateTime.UnixEpoch
            Comment = ""
        }

[<Json.AutoCodec(false)>]
type LegacyScoreDatabase =
    {
        Entries: ConcurrentDictionary<string, LegacyChartSaveData>
    }
    static member Default =
        {
            Entries = new ConcurrentDictionary<string, LegacyChartSaveData>()
        }

    static member TryLoad() : LegacyScoreDatabase option =
        JSON.FromFile(Path.Combine(get_game_folder "Data", "scores.json"))
        |> Result.toOption

    static member MarkOld() =
        File.Move(
            Path.Combine(get_game_folder "Data", "scores.json"),
            (Path.Combine(get_game_folder "Data", "scores.old"))
        )
