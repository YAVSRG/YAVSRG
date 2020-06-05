namespace Prelude.Data

open System
open System.IO
open System.Collections.Generic
open Newtonsoft.Json
open Prelude.Common
open Prelude.Json
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors

module Profiles = 

    type ProfileStats() =
        member val Plays = 0 with get, set

    type Profile = {
        UUID: string
        Stats: ProfileStats
        Name: Setting<string>
        ScrollSpeed: Setting<float>
        HitPosition: Setting<int>
        HitLighting: Setting<bool>
        Upscroll: Setting<bool>
        BackgroundDim: Setting<float>
        PerspectiveTilt: Setting<float>
        ScreenCoverUp: Setting<float>
        ScreenCoverDown: Setting<float>
        ScreenCoverFadeLength: Setting<int>
        ColorStyle: Setting<ColorConfig>
        KeymodePreference: Setting<int>
        UseKeymodePreference: Setting<bool>
        NoteSkin: Setting<string>

        SelectedHPSystem: Setting<int>
        SelectedAccSystem: Setting<int>
        //pacemaker, score saving, fail behaviour

        ChartSortMode: Setting<string>
        ChartGroupMode: Setting<string>
        ChartColorMode: Setting<string>

        Playstyles: Layout array
    } with
        static member Default = {
            UUID = Guid.NewGuid().ToString()
            Stats = ProfileStats()

            Name = StringSetting("Default Profile", false)

            ScrollSpeed = FloatSetting(2.05, 1.0, 3.0)
            HitPosition = IntSetting(0, -100, 400)
            HitLighting = Setting(false)
            Upscroll = Setting(false)
            ScreenCoverUp = FloatSetting(0.0, 0.0, 1.0)
            ScreenCoverDown = FloatSetting(0.0, 0.0, 1.0)
            ScreenCoverFadeLength = IntSetting(200, 0, 500)
            PerspectiveTilt = FloatSetting(0.0, -1.0, 1.0)
            BackgroundDim = FloatSetting(0.5, 0.0, 1.0)
            NoteSkin = Setting("default")
            SelectedHPSystem = Setting(0)
            SelectedAccSystem = Setting(0)
            ColorStyle = Setting(ColorConfig.Default)
            KeymodePreference = IntSetting(4, 3, 10)
            UseKeymodePreference = Setting(false)
            Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]

            ChartSortMode = Setting("Title")
            ChartGroupMode = Setting("Pack")
            ChartColorMode = Setting("Nothing")
        }

    module Profile =

        let profilePath = 
            let pp = Path.Combine(getDataPath("Data"), "Profiles")
            Directory.CreateDirectory(pp) |> ignore
            pp
        
        let save (p: Profile) = Path.Combine(profilePath, p.UUID + ".json") |> JsonHelper.saveFile p

        let load (path: string): Profile = JsonHelper.loadFile path