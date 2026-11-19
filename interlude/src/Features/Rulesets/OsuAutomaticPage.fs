namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Options
open Interlude.UI
open Interlude.Options

type OsuAutomaticPage() =
    inherit Page()

    let confirm () =
        options.Ruleset.Set(OsuAutomatic(options.AutomaticOsuOD.Value))
        Menu.Back()

    override this.Content() : Widget =
        page_container()
            .With(
                PageSetting(%"ruleset.osu_automatic.default_od", Slider(options.AutomaticOsuOD, Step = 0.1f)).Pos(0),
                PageButton.Once(%"confirm.yes", confirm).Pos(3)
            )

    override this.Title = %"rulesets.osu_automatic"
