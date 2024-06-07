namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI.Menu

type EditThemePage() =
    inherit Page()

    let data = Content.ThemeConfig

    let name = Setting.simple data.Name

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"themes.edittheme.themename", name)
            .Pos(0)
        :> Widget

    override this.Title = data.Name

    override this.OnClose() =
        Themes.save_config { data with Name = name.Value }
