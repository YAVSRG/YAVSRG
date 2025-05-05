namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Interlude.UI
open Interlude.Content

type ConfirmUnlinkedImportPage(path: string) =
    inherit Page()

    let info =
        Callout.Normal
            .Icon(Icons.ALERT_CIRCLE)
            .Title(%"unlinkedsongsimport.info.title")
            .Body(%"unlinkedsongsimport.info.body")

    override this.Content() =
        page_container()
            .With(
                // todo: go straight to library page
                PageButton.Once(%"unlinkedsongsimport.link_intended", fun () -> Menu.Back())
                    .Pos(8),
                PageButton.Once(%"unlinkedsongsimport.confirm", fun () ->
                    let task_tracking = TaskTracking.add (Path.GetFileName path)
                    let task = Imports.auto_detect_import(path, Content.Charts, Content.UserData, task_tracking.set_Progress)
                    import_queue.Request(task,
                        function
                        | Ok result ->
                            Notifications.task_feedback (
                                Icons.CHECK,
                                %"notification.import_success",
                                [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                            )
                            Content.TriggerChartAdded()
                        | Error reason ->
                            Logging.Error "Error importing %s: %s" path reason
                            Notifications.error (%"notification.import_failed", reason)
                    )

                    Menu.Back()
                )
                    .Pos(11),
                CalloutCard(info).Position(fun (w, h) -> Position.Box(0.0f, 0.0f, 0.0f, -10.0f, w, h))
            )

    override this.Title = %"unlinkedsongsimport"