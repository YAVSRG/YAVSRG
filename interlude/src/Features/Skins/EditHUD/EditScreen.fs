namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Pacemaker
open Interlude.Features.Skins

type PositionerInfo(ctx: PositionerContext) =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O3, Border = K Colors.cyan_accent)

    let mutable currently_on_right = true

    let LEFT_POSITION : Position = Position.SliceY(400.0f).SliceL(375.0f)
    let RIGHT_POSITION : Position = Position.SliceY(400.0f).SliceR(375.0f)

    let dropdown_wrapper =
        DropdownWrapper(
            (fun d ->
                if currently_on_right then
                    Position.SliceY(d.Height).BorderL(370.0f).Translate(-10.0f, 0.0f)
                else
                    Position.SliceY(d.Height).BorderR(370.0f).Translate(10.0f, 0.0f)
            ),
            FocusTrap = true
        )

    override this.Init(parent) =
        this |* dropdown_wrapper

        NavigationContainer.Column()
        |+ Button(
            (fun () -> Icons.LIST + " " + HudElement.name ctx.Selected),
            this.ToggleElementDropdown,
            Hotkey = "context_menu",
            Position = Position.SliceT(60.0f).Shrink(20.0f, 5.0f)
        )
        |+ Text(%"hud.editor.elements_hint", Color = K Colors.text_subheading, Position = Position.Row(50.0f, 30.0f).Shrink(20.0f, 0.0f))
        |+ Button(
            (fun () -> if (HudElement.enabled_setting ctx.Selected).Value then Icons.CHECK_CIRCLE + " " + %"hud.editor.enabled" else Icons.CIRCLE + " " + %"hud.editor.disabled"),
            (fun () ->
                Setting.app not (HudElement.enabled_setting ctx.Selected)
                GameThread.defer (fun () -> ctx.Create ctx.Selected)
            ),
            Disabled = (fun () -> HudElement.can_toggle ctx.Selected |> not),
            Position = Position.Row(100.0f, 60.0f).Shrink(10.0f, 5.0f)
        )
        |+ Button(
            Icons.REFRESH_CW + " " + %"hud.editor.reset_position",
            (fun () ->
                HudElement.position_setting(ctx.Selected).Set(HudElement.default_position ctx.Selected)
                GameThread.defer (fun () -> ctx.Create ctx.Selected)
            ),
            Position = Position.Row(160.0f, 60.0f).Shrink(10.0f, 5.0f)
        )
        |+ Button(
            Icons.SETTINGS + " " + %"hud.editor.element_options",
            (fun () -> show_menu ctx.Selected (fun () -> ctx.Create ctx.Selected)),
            Hotkey = "options",
            Disabled = (fun () -> HudElement.can_configure ctx.Selected |> not),
            Position = Position.Row(220.0f, 60.0f).Shrink(10.0f, 5.0f)
        )
        |+ Button(
            Icons.ANCHOR + " " + %"hud.editor.anchor",
            this.ToggleAnchorDropdown,
            Position = Position.Row(280.0f, 60.0f).Shrink(10.0f, 5.0f)
        )
        |+ Button(
            Icons.EDIT + " " + %"hud.editor.advanced",
            (fun () -> EditHUDPage().Show()),
            Position = Position.Row(340.0f, 60.0f).Shrink(10.0f, 5.0f)
        )
        |> this.Add

        this.Position <- RIGHT_POSITION
        base.Init parent

    member private this.ToggleElementDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = HudElement.FULL_LIST |> List.map (fun e -> e, HudElement.name e)
                    ColorFunc = K Colors.text
                    Setting =
                        Setting.make
                            (fun v -> ctx.Select v)
                            (fun () -> ctx.Selected)
                }
        )

    member private this.ToggleAnchorDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            DropdownMenu
                {
                    Items = [
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.CENTER)), %"hud.editor.relative_to.playfield_center"
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.LEFT)), %"hud.editor.relative_to.playfield_left"
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.RIGHT)), %"hud.editor.relative_to.playfield_right"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.CENTER)), %"hud.editor.relative_to.screen_center"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.LEFT)), %"hud.editor.relative_to.screen_left"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.RIGHT)), %"hud.editor.relative_to.screen_right"
                    ]
                }
            |+ Text(%"hud.editor.relative_to", Position = Position.BorderT 40.0f)
        )

    override this.Update(elapsed_ms, moved) =
        let mutable moved = moved

        if ctx.Positioners.ContainsKey ctx.Selected then
            if
                currently_on_right
                && (ctx.Positioners.[ctx.Selected].Bounds.Intersect this.Bounds).Visible
                && ctx.Positioners.[ctx.Selected].Bounds.Left > 400.0f
            then
                currently_on_right <- false
                moved <- true
                this.Position <- LEFT_POSITION
            elif
                not currently_on_right
                && (ctx.Positioners.[ctx.Selected].Bounds.Intersect this.Bounds).Visible
                && ctx.Positioners.[ctx.Selected].Bounds.Right < this.Parent.Bounds.Right - 400.0f
            then
                currently_on_right <- true
                moved <- true
                this.Position <- RIGHT_POSITION

        base.Update(elapsed_ms, moved)

module EditHudScreen =

    let edit_hud_screen (chart: Chart, with_colors: ColoredChart, on_exit) =

        let replay_data: IReplayProvider =
            StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let mutable replay_data = replay_data

        let mutable scoring =
            ScoreProcessor.create ruleset with_colors.Keys replay_data with_colors.Source.Notes SelectedChart.rate.Value

        let mutable time = -Time.infinity

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)
            scoring <- ScoreProcessor.create ruleset with_colors.Keys replay_data with_colors.Source.Notes SelectedChart.rate.Value
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerState.None, scoring) with
            override this.AddWidgets() =

                let ctx =
                    {
                        Screen = Container(NodeType.None)
                        Playfield = this.Playfield
                        State = this.State
                        Selected = HudElement.Accuracy
                        Positioners = Map.empty
                    }

                HudElement.FULL_LIST
                |> Seq.iter ctx.Create

                this
                |+ ctx.Screen
                |* PositionerInfo ctx
            // todo: way to turn on multiplayer player list

            override this.OnEnter p =
                DiscordRPC.in_menus ("Customising HUD")
                Dialog.close ()
                Background.dim (float32 options.BackgroundDim.Value)
                Toolbar.hide ()
                Song.on_finish <- SongFinishAction.LoopFromBeginning
                Input.remove_listener ()

            override this.OnExit s =
                base.OnExit s
                if s <> Screen.Type.Play then on_exit ()

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if chart_time < time then
                    seek_backwards this

                time <- chart_time

                base.Update(elapsed_ms, moved)

                if not replay_data.Finished then
                    scoring.Update chart_time
                else
                    Song.seek 0.0f<ms>
        }