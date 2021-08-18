namespace Interlude.UI.Screens.LevelSelect

open System.Drawing
open Prelude.Common
open Prelude.Data.ChartManager
open Prelude.Scoring
open Interlude.UI
open Interlude.Gameplay

type PersonalBestData = PersonalBests<float * int> option * PersonalBests<Lamp> option * PersonalBests<bool> option
    
[<Struct>]
type Navigation =
| Nothing
| Backward of string * CachedChart
| Forward of bool
    
[<Struct>]
type ScrollTo =
| Nothing
| ScrollToChart
| ScrollToPack of string

module private Globals =
    
    //functionality wishlist:
    // - hotkeys to navigate by pack/close and open quickly
    // - display of keycount for charts
    // - "random chart" hotkey
    // - cropping of text that is too long
        
    //eventual todo:
    // - goals and playlists editor

    
    let mutable selectedGroup = ""
    let mutable selectedChart = "" //filepath
    let mutable expandedGroup = ""
    
    let mutable scrollBy : float32 -> unit = ignore
    let mutable colorVersionGlobal = 0
    //future todo: different color settings?
    let mutable colorFunc = fun ((_, _, _): PersonalBestData) -> Color.FromArgb(40, 200, 200, 200)
    
    //updated whenever screen refreshes
    let mutable scoreSystem = "SC+ (J4)"
    let mutable hpSystem = "VG"
    
    let mutable scrollTo = ScrollTo.Nothing
    let mutable navigation = Navigation.Nothing
    
    let switchCurrentChart(cc, groupName) =
        match cache.LoadChart cc with
        | Some c ->
            changeChart(cc, c)
            selectedChart <- cc.FilePath
            expandedGroup <- groupName
            selectedGroup <- groupName
            scrollTo <- ScrollToChart
        | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
    
    //todo: move to Gameplay
    let playCurrentChart() =
        if currentChart.IsSome then ScreenGlobals.newScreen((fun () -> new PlayScreen(PlayScreenType.Normal) :> Screen), ScreenType.Play, ScreenTransitionFlag.Default)
        else Logging.Warn "Tried to play selected chart; There is no chart selected"