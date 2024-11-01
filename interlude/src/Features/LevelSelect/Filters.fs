namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type FiltersPage() =
    inherit Page()

    override this.Content() = 
        page_container()
        |+ PageSetting("placeholder", Slider(Setting.bounded (0.0f, 1.0f) 0.5f))
        :> Widget

    override this.Title = %"levelselect.filters"
    override this.OnClose() =
        ()