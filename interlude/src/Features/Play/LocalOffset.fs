namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data.User
open Prelude.Gameplay.Scoring
open Interlude.UI
open Interlude.Features.Gameplay

module LocalOffset =

    let offset_setting (save_data: ChartSaveData) =
        Setting.make save_data.set_Offset save_data.get_Offset
        |> Setting.roundf_uom 0
        |> Setting.trigger Song.set_local_offset

    let get_automatic (state: PlayState) (save_data: ChartSaveData) =
        let mutable sum = 0.0f<ms / rate>
        let mutable count = 1.0f

        for ev in state.Scoring.Events do
            match ev.Action with
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

type LocalOffsetPage(state: PlayState, save_data: ChartSaveData, setting: Setting<float32<ms>>, on_close: unit -> unit) =
    inherit Page()

    let offset_slider =
        Slider(
            setting
            |> Setting.map float32 (fun x -> x * 1.0f<ms>)
            |> Setting.bound (-200.0f, 200.0f),
            Step = 1f,
            Format = (fun v -> sprintf "%.0fms" v)
        )

    let recommended_offset = LocalOffset.get_automatic state save_data

    let apply_recommended() =
        setting.Set recommended_offset
        Menu.Back()

    let reset_offset() =
        setting.Set 0.0f<ms>

    override this.Content() =
        RenderThread.defer (fun () -> offset_slider.Select false)

        page_container()
        |+ PageSetting(
            %"play.localoffset",
            offset_slider
        )
            .Pos(0)
        |+ Text(%"play.localoffset.slider_hint", Align = Alignment.LEFT).Pos(2, 1)
        |+ PageButton(
                [sprintf "%.0fms" recommended_offset] %> "play.localoffset.use_recommended",
                apply_recommended,
                Hotkey = %%"accept_offset"
        )
            .Pos(3)
        |+ PageButton(%"play.localoffset.reset", reset_offset, Hotkey = %%"reset_offset").Pos(5)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        if offset_slider.Selected && (%%"select").Tapped() then
            Menu.Back()

        base.Update(elapsed_ms, moved)


    override this.Title = %"play.localoffset"

    override this.OnClose() =
        on_close()