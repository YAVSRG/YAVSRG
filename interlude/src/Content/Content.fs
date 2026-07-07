namespace Interlude.Content

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Prelude.Skins
open Prelude.Skins.HudLayouts
open Prelude.Skins.Noteskins
open Prelude.Skins.Themes
open Prelude.Backbeat
open Prelude.Data.Library
open Prelude.Data.User
open Prelude.Data.User.Stats

type Content =

    static member Init() : unit =
        Themes.init()
        Skins.init()
        Sounds.init()

    static member LoadData() : unit =
        Tables.init()
        Rulesets.init()
        Data.init()

    static member Deinit() : unit = Data.deinit()

    static member Library : Library = Data.library
    static member UserData : UserDatabase = Data.library.UserData
    static member Charts : ChartDatabase = Data.library.Charts
    static member Collections : Collections = Data.library.Collections
    static member Stats : Stats = Data.stats

    static member Table : Table option = Tables.current
    static member ThemeConfig : ThemeConfig = Themes.current_config
    static member Theme : Theme = Themes.current
    static member NoteskinConfig : NoteskinConfig = Skins.current_noteskin.Config
    static member Noteskin : Noteskin = Skins.current_noteskin
    static member NoteskinMeta : SkinMetadata = Skins.current_noteskin_meta
    static member HUD : HudConfig = Skins.current_hud.Config
    static member HUDMeta : SkinMetadata = Skins.current_hud_meta
    static member Texture(id: string) : Sprite = Sprites.get(id)

    static member OnChartAdded = Data.charts_updated
    static member TriggerChartAdded() : unit = GameThread.on_game_thread Data.charts_updated_ev.Trigger