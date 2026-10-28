namespace Interlude.Features.Import

open System.IO
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Imports
open Interlude.UI
open Interlude.Content
open Prelude.Formats.Conversions

type ConfirmUnlinkedImportPage(path: string) =
    inherit Page()

    // todo: remove duplication
    let conversion_result_callback (result: Result<ConversionResult, string>) : unit =
        match result with
        | Ok result ->
            Notifications.task_feedback(
                Icons.CHECK,
                %"notification.import_success",
                [ result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString() ]
                %> "notification.import_success.body"
            )

            Content.TriggerChartAdded(result.FirstConvertedChart)
        | Error reason -> Notifications.error(%"notification.import_failed", reason)

    let info =
        Callout.Normal
            .Icon(Icons.ALERT_CIRCLE)
            .Title(%"unlinkedsongsimport.info.title")
            .Body(%"unlinkedsongsimport.info.body")

    override this.Content() : Widget =
        page_container()
            .With(
                // todo: go straight to library page
                PageButton.Once(%"unlinkedsongsimport.link_intended", fun () -> Menu.Back()).Pos(8),
                PageButton
                    .Once(
                        %"unlinkedsongsimport.confirm",
                        fun () ->
                            FileSystemImportTask(path, Content.Library)
                                .Track(TaskTracking.add(Path.GetFileName(path)))
                                .WithCallback(
                                    function
                                    | Ok(Some result) -> conversion_result_callback(Ok(result))
                                    | Ok(None) ->
                                        Notifications.error(
                                            %"notification.import_failed",
                                            %"notification.import_failed.not_importable"
                                        )
                                    | Error(reason) -> conversion_result_callback(Error(reason))
                                )
                                .Queue(import_queue)

                            Menu.Back()
                    )
                    .Pos(11),
                CalloutCard(info).Position(fun (w, h) -> Position.Box(0.0f, 0.0f, 0.0f, -10.0f, w, h))
            )

    override this.Title = %"unlinkedsongsimport"
