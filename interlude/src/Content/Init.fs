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

type Content() =

    static member init () =
        Themes.init ()
        Skins.init ()
        Sounds.init ()

    static member load_data () =
        Tables.init ()
        Rulesets.init ()
        Data.init ()

    static member deinit () = Data.deinit ()

    static member UserData : UserDatabase = Data.user_db
    static member Library : Library = Data.library
    static member Charts : ChartDatabase = Data.library.Charts
    static member Collections : Collections = Data.library.Collections

    static member Table : Table option = Tables.current
    static member ThemeConfig : ThemeConfig = Themes.current_config
    static member Theme : Theme = Themes.current
    static member NoteskinConfig : NoteskinConfig = Skins.current_noteskin.Config
    static member Noteskin : Noteskin = Skins.current_noteskin
    static member NoteskinMeta : SkinMetadata = Skins.current_noteskin_meta
    static member HUD : HudConfig = Skins.current_hud.Config
    static member HUDMeta : SkinMetadata = Skins.current_hud_meta
    static member Texture(id: string) : Sprite = Sprites.get id

    static member OnChartAdded = Data.charts_updated
    static member TriggerChartAdded() = GameThread.on_game_thread Data.charts_updated_ev.Trigger