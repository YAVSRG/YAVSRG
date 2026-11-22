namespace Prelude.Data.OsuClientInterop

open System.Runtime.CompilerServices
open Prelude
open Prelude.Mods

type OsuMods =
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

type OsuModsExtensions =

    static member val private AllowedMods: OsuMods =
        OsuMods.NoFail
        ||| OsuMods.Easy
        ||| OsuMods.Hidden
        ||| OsuMods.HardRock
        ||| OsuMods.SuddenDeath
        ||| OsuMods.DoubleTime
        ||| OsuMods.HalfTime
        ||| OsuMods.Nightcore
        ||| OsuMods.Flashlight
        ||| OsuMods.Perfect
        ||| OsuMods.FadeIn
        ||| OsuMods.ScoreV2
        ||| OsuMods.Mirror

    [<Extension>]
    static member ConvertToRateAndMods(mods: OsuMods) : (Rate * ModState) option =
        if mods &&& OsuModsExtensions.AllowedMods <> mods then
            None
        else

            let rate =
                if mods &&& OsuMods.DoubleTime = OsuMods.DoubleTime then 1.5f<rate>
                elif mods &&& OsuMods.HalfTime = OsuMods.HalfTime then 0.75f<rate>
                else 1.0f<rate>

            let mod_state =
                seq {
                    if mods &&& OsuMods.Mirror = OsuMods.Mirror then
                        yield ("mirror", 0L)
                }
                |> Map.ofSeq

            Some(rate, mod_state)
