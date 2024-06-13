namespace Interlude.Features.OptionsMenu.Debug

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type DebugPage() =
    inherit Page()

    let themes = PageSetting(%"themes.theme", Dummy())

    let refresh () =
        themes.Child <- SelectDropdown(Themes.list (), options.Theme)

    override this.Content() =
        refresh ()

        page_container()
        |+ PageButton
            .Once(
                %"debug.rebuildcache",
                fun () ->
                    Caching.Cache.recache_service.Request(
                        Content.Cache,
                        fun () ->
                            Notifications.task_feedback (Icons.FOLDER, %"notification.recache_complete", "")
                    )

                    Notifications.action_feedback (Icons.FOLDER, %"notification.recache", "")
            )
            .Tooltip(Tooltip.Info("debug.rebuildcache"))
            .Pos(0)
        |+ themes
            .Tooltip(Tooltip.Info("themes.theme"))
            .Pos(3)
        |+ PageButton(%"themes.showthemesfolder", (fun () -> open_directory (get_game_folder "Themes")))
            .Tooltip(Tooltip.Info("themes.showthemesfolder"))
            .Pos(5)
        :> Widget

    override this.Title = %"debug"
    override this.OnClose() = ()
