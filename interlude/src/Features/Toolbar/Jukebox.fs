namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Content

type Jukebox() =
    inherit Container(NodeType.None)

    let song_title_carousel = Animation.Counter(10000000.0)
    let CAROUSEL_SPEED = 0.05
    let CAROUSEL_GAP = 50.0f

    override this.Init(parent) =
        this 
        |+ Button(Icons.SKIP_BACK, 
            (fun () -> 
                match Things.previous() with
                | None -> ()
                | Some cc -> SelectedChart.change(cc, LibraryContext.None, true)
            ),
            Disabled = (Things.has_previous >> not),
            Position = Position.Margin(5.0f).SliceLeft(45.0f)
        )
        |+ Button(Icons.PAUSE, 
            (fun () -> if Song.playing() then Song.pause() else Song.resume()),
            Position = Position.Margin(5.0f).SliceLeft(45.0f).Translate(45.0f, 0.0f)
        )
        |* Button(Icons.SKIP_FORWARD,
            (fun () ->
                let ctx : LibraryViewContext =
                    {
                        Rate = SelectedChart.rate.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        ScoreDatabase = Content.Scores
                    }

                match Suggestion.get_random [] ctx with
                | None -> ()
                | Some cc ->
                    Things.add_current_chart_to_history()
                    SelectedChart.change(cc, LibraryContext.None, true)
            ),
            Position = Position.Margin(5.0f).SliceLeft(45.0f).Translate(90.0f, 0.0f)
        )
        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds (Colors.shadow_1.O2)

        base.Draw()

        let song_title = 
            match SelectedChart.CACHE_DATA with 
            | Some cc -> cc.Artist + " - " + cc.Title
            | None -> %"jukebox.no_chart_selected"

        let song_title_width = Text.measure(Style.font, song_title) * 25.0f

        let carousel_bounds = this.Bounds.TrimLeft(135.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING)

        Stencil.start_stencilling false
        Draw.rect carousel_bounds Color.Transparent
        Stencil.start_drawing ()
        
        if song_title_width + CAROUSEL_GAP < carousel_bounds.Width then
            Text.draw_b(Style.font, song_title, 25.0f, carousel_bounds.Left + (carousel_bounds.Width - song_title_width) * 0.5f, this.Bounds.Top + 7.0f, Colors.text_subheading)
        else
            let x = float32 (song_title_carousel.Time * CAROUSEL_SPEED) % (song_title_width + CAROUSEL_GAP)
            Text.draw_b(Style.font, song_title, 25.0f, carousel_bounds.Left - x, this.Bounds.Top + 7.0f, Colors.text_subheading)
            Text.draw_b(Style.font, song_title, 25.0f, carousel_bounds.Left + song_title_width + CAROUSEL_GAP - x, this.Bounds.Top + 7.0f, Colors.text_subheading)

        Stencil.finish()


    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        song_title_carousel.Update elapsed_ms

