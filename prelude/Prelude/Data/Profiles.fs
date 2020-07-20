namespace Prelude.Data

open System
open System.IO
open System.Collections.Generic
open Newtonsoft.Json
open Prelude.Common
open Prelude.Json
open Prelude.Gameplay.Score
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors
open Prelude.Data.ScoreManager

module Profiles = 

    type ScoreSaving =
    | Always = 0
    | Pacemaker = 1
    | PB = 2

    type Pacemaker =
    | Accuracy of float
    | Lamp of Lamp

    type FailType =
    | Instant
    | AtEnd

    type ProfileStats = {
        //todo: rrd graph of improvement over time/session performances
        TopPhysical: Setting<TopScore list>
        TopTechnical: Setting<TopScore list>
    } with
        static member Default = {
            TopPhysical = Setting([])
            TopTechnical = Setting([])
        }

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

        Playstyles: Layout array

        SelectedHPSystem: Setting<int> //todo: not this
        SelectedAccSystem: Setting<int>
        ScoreSaveCondition: Setting<ScoreSaving>
        FailCondition: Setting<FailType>
        Pacemaker: Setting<Pacemaker>

        ChartSortMode: Setting<string>
        ChartGroupMode: Setting<string>
        ChartColorMode: Setting<string>
    } with
        static member Default = {
            UUID = Guid.NewGuid().ToString()
            Stats = ProfileStats.Default

            Name = StringSetting("Default Profile", false)

            ScrollSpeed = FloatSetting(2.05, 1.0, 3.0)
            HitPosition = IntSetting(0, -100, 400)
            HitLighting = Setting(false)
            Upscroll = Setting(false)
            BackgroundDim = FloatSetting(0.5, 0.0, 1.0)
            PerspectiveTilt = FloatSetting(0.0, -1.0, 1.0)
            ScreenCoverUp = FloatSetting(0.0, 0.0, 1.0)
            ScreenCoverDown = FloatSetting(0.0, 0.0, 1.0)
            ScreenCoverFadeLength = IntSetting(200, 0, 500)
            NoteSkin = Setting("default")
            ColorStyle = Setting(ColorConfig.Default)
            KeymodePreference = IntSetting(4, 3, 10)
            UseKeymodePreference = Setting(false)
            Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]

            SelectedHPSystem = Setting(0)
            SelectedAccSystem = Setting(0)
            ScoreSaveCondition = Setting(ScoreSaving.Always)
            FailCondition = Setting(AtEnd)
            Pacemaker = Setting(Accuracy 0.95)

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