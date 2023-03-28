namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Prelude.Scoring
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features

module Gameplay = 

    type ScreencoverPage() as this =
        inherit Page()

        let preview = Themes.NoteskinPreview 0.35f

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.lanecover.enabled", Selector<_>.FromBool options.LaneCover.Enabled).Pos(200.0f)
                |+ PrettySetting("gameplay.lanecover.hidden", Slider<_>.Percent(options.LaneCover.Hidden, 0.01f)).Pos(350.0f)
                |+ PrettySetting("gameplay.lanecover.sudden", Slider<_>.Percent(options.LaneCover.Sudden, 0.01f)).Pos(450.0f)
                |+ PrettySetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, 0.01f)).Pos(550.0f)
                |+ PrettySetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true)).Pos(650.0f, PRETTYWIDTH, PRETTYHEIGHT * 2.0f)
                |+ preview
            )
        override this.Title = L"options.gameplay.lanecover.name"
        override this.OnDestroy() = preview.Destroy()
        override this.OnClose() = ()

    type GameplayPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Pos(200.0f)
                |+ PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Pos(280.0f)
                |+ PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Pos(360.0f)
                |+ PrettySetting("gameplay.backgrounddim", Slider<_>.Percent(options.BackgroundDim, 0.01f)).Pos(440.0f)
                |+ PrettyButton("gameplay.lanecover", fun() -> Menu.ShowPage ScreencoverPage).Pos(520.0f)
                |+ PrettyButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage).Pos(670.0f)
                |+ PrettyButton("gameplay.rulesets", fun () -> Menu.ShowPage Rulesets.FavouritesPage).Pos(750.0f)
            )
        override this.Title = L"options.gameplay.name"
        override this.OnClose() = ()
