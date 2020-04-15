namespace Prelude.Data

open System
open System.Collections.Generic
open Newtonsoft.Json
open Prelude.Common
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors

module Profiles = 

    type ProfileStats() =
        member val Plays = 0 with get, set
    
    type ProfileData = {
        [<JsonRequired>] UUID: string
        Stats: ProfileStats option
        EnabledThemes: List<string>
        Playstyles: Layout array

        Name: string option
        ScrollSpeed: float option
        HitPosition: int option
        HitLighting: bool option
        Upscroll: bool option
        BackgroundDim: float option
        PerspectiveTilt: float option
        ScreenCoverUp: float option
        ScreenCoverDown: float option
        ScreenCoverFadeLength: int option
        ColorStyle: ColorConfig option
        KeymodePreference: int option
        UseKeymodePreference: bool option
        NoteSkin: string option
        ChartSortMode: string option
        ChartGroupMode: string option
        ChartColorMode: string option
    }

    type Profile = {
        UUID: string
        Stats: ProfileStats
        EnabledThemes: List<string>
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

        //pacemaker, score saving, fail behaviour
        //choice of score system, hp system

        ChartSortMode: Setting<string>
        ChartGroupMode: Setting<string>
        ChartColorMode: Setting<string>

        Playstyles: Layout array
    }
    with
        static member Default = {
            UUID = Guid.NewGuid().ToString()
            Stats = ProfileStats()
            EnabledThemes = new List<string>()

            Name = StringSetting("Default Profile", false)

            ScrollSpeed = NumSetting(2.05, 1.0, 3.0)
            HitPosition = NumSetting(0, -100, 400)
            HitLighting = Setting(false)
            Upscroll = Setting(false)
            ScreenCoverUp = NumSetting(0.0, 0.0, 1.0)
            ScreenCoverDown = NumSetting(0.0, 0.0, 1.0)
            ScreenCoverFadeLength = NumSetting(200, 0, 500)
            PerspectiveTilt = NumSetting(0.0, -1.0, 1.0)
            BackgroundDim = NumSetting(0.5, 0.0, 1.0)
            NoteSkin = Setting("default")
            ColorStyle = Setting(ColorConfig.Default)
            KeymodePreference = NumSetting(4, 3, 10)
            UseKeymodePreference = Setting(false)
            Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]

            ChartSortMode = Setting("Title")
            ChartGroupMode = Setting("Pack")
            ChartColorMode = Setting("Nothing")
        }
        member this.FromData (pd: ProfileData) =
            let f (obj: 'T option) (setting: Setting<'T>) =
                if obj.IsSome then setting.Set(obj.Value)
            f pd.Name this.Name
            f pd.NoteSkin this.NoteSkin
            f pd.ChartSortMode this.ChartSortMode
            f pd.ChartGroupMode this.ChartGroupMode
            f pd.ChartColorMode this.ChartColorMode
            f pd.ColorStyle this.ColorStyle
            f pd.ScrollSpeed this.ScrollSpeed
            f pd.HitPosition this.HitPosition
            f pd.HitLighting this.HitLighting
            f pd.Upscroll this.Upscroll
            f pd.ScreenCoverUp this.ScreenCoverUp
            f pd.ScreenCoverDown this.ScreenCoverDown
            f pd.ScreenCoverFadeLength this.ScreenCoverFadeLength
            f pd.PerspectiveTilt this.PerspectiveTilt
            f pd.BackgroundDim this.BackgroundDim
            f pd.KeymodePreference this.KeymodePreference
            f pd.UseKeymodePreference this.UseKeymodePreference

            { this with
                UUID = pd.UUID; Stats = (Option.defaultValue this.Stats pd.Stats)
                Playstyles = (if pd.Playstyles <> null then pd.Playstyles else this.Playstyles)
                EnabledThemes = (if pd.EnabledThemes <> null then pd.EnabledThemes else this.EnabledThemes )}
            
