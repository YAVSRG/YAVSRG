namespace Prelude.Data.OsuClientInterop

open System.Runtime.CompilerServices
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

type ModsExtensions =

    static member val private AllowedMods: Mods =
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

    [<Extension>]
    static member ConvertToRateAndMods(mods: Mods) : (Rate * ModState) option =
        if mods &&& ModsExtensions.AllowedMods <> mods then
            None
        else

            let rate =
                if mods &&& Mods.DoubleTime = Mods.DoubleTime then 1.5f<rate>
                elif mods &&& Mods.HalfTime = Mods.HalfTime then 0.75f<rate>
                else 1.0f<rate>

            let mod_state =
                seq {
                    if mods &&& Mods.Mirror = Mods.Mirror then
                        yield ("mirror", 0L)
                }
                |> Map.ofSeq

            Some(rate, mod_state)
