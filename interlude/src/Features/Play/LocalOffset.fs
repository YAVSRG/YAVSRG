namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data
open Prelude.Gameplay
open Interlude.UI
open Interlude.Features.Gameplay

module LocalOffset =

    let offset_setting (save_data: ChartSaveData) =
        Setting.make save_data.set_Offset save_data.get_Offset
        |> Setting.roundt 0
        |> Setting.trigger Song.set_local_offset

    let get_automatic (state: PlayState) (save_data: ChartSaveData) =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f

        for ev in state.Scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count * SelectedChart.rate.Value

        if count < 10.0f then
            save_data.Offset
        else
            save_data.Offset - mean * 1.25f

    let apply_automatic (state: PlayState) (save_data: ChartSaveData) =
        let setting = offset_setting save_data
        setting.Value <- get_automatic state save_data

type LocalOffsetPage(state: PlayState, save_data: Data.ChartSaveData, setting: Setting<float32<ms>>, on_close: unit -> unit) =
    inherit Page()

    let offset_slider =
        PageSetting(
            %"play.localoffset",
            Slider(
                setting
                |> Setting.map float32 (fun x -> x * 1.0f<ms>)
                |> Setting.bound -200.0f 200.0f,
                Step = 1f,
                Format = (fun v -> sprintf "%.0fms" v)
            )
        )

    let recommended_offset = LocalOffset.get_automatic state save_data

    let apply_recommended() =
        setting.Set recommended_offset
        Menu.Back()

    override this.Content() =
        defer (fun () -> offset_slider.Select false)

        page_container()
        |+ offset_slider.Pos(0)
        |+ Text(%"play.localoffset.slider_hint", Align = Alignment.LEFT).Pos(2, 1)
        |+ (
            PageButton(
                [sprintf "%.0fms" recommended_offset] %> "play.localoffset.use_recommended",
                apply_recommended
            )
            |+ Text(sprintf "%s: %O" (%"misc.hotkeyhint") (%%"accept_suggestion"),
                Color = K Colors.text_cyan,
                Align = Alignment.RIGHT,
                Position = Position.Shrink(10.0f, 5.0f)
            )
        ).Pos(3)
        |+ HotkeyAction("accept_suggestion", apply_recommended)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        if offset_slider.Focused && (%%"select").Tapped() then
            Menu.Back()

        base.Update(elapsed_ms, moved)


    override this.Title = %"play.localoffset"

    override this.OnClose() =
        on_close()