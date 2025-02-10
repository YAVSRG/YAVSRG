namespace Interlude.Content

open Percyqaz.Flux.Windowing

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

    static member UserData = Data.user_db
    static member Library = Data.library
    static member Charts = Data.library.Charts
    static member Collections = Data.library.Collections

    static member Table = Tables.current
    static member ThemeConfig = Themes.current_config
    static member Theme = Themes.current
    static member NoteskinConfig = Skins.current_noteskin.Config
    static member Noteskin = Skins.current_noteskin
    static member NoteskinMeta = Skins.current_noteskin_meta
    static member HUD = Skins.current_hud.Config
    static member HUDMeta = Skins.current_hud_meta
    static member Texture(id: string) = Sprites.get id

    static member OnChartAdded = Data.charts_updated
    static member TriggerChartAdded() = GameThread.on_game_thread Data.charts_updated_ev.Trigger