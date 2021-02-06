namespace Interlude.UI

open OpenTK
open System.IO
open Percyqaz.Json
open Prelude.Common
open Prelude.Web
open Interlude
open Interlude.Utils
open Interlude.UI.Components

module FileDropHandling =
    let import(path: string) =
        TaskManager.AddTask("Import " + Path.GetFileName(path), Gameplay.cache.AutoConvert(path), (fun b -> ScreenLevelSelect.refresh <- ScreenLevelSelect.refresh || b), true)

type ImportCard(name, url) as this =
    inherit Widget()
    do
        this.Add(new TextBox(K name, K (Color.White, Color.Black), 0.5f))
        this.Add(
            new Clickable(
                (fun () ->
                    let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".zip")
                    TaskManager.AddTask("Downloading " + name, downloadFile(url, target),
                        (fun b ->
                            if b then
                                TaskManager.AddTask("Importing " + name, Gameplay.cache.AutoConvert(target), (fun b -> ScreenLevelSelect.refresh <- ScreenLevelSelect.refresh || b; File.Delete(target)), true)), true)), ignore))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 40.0f, 0.0f)

[<Json.AllRequired>]
type EOPack = {
    ``type``: string
    id: int
    attributes: {|name: string; average: float; download: string; size: int64|}
}
with
    static member Default = {``type`` = "pack"; id = 0; attributes = {|name = ""; average = 0.0; download = ""; size = 0L|}}

type ScreenImport() as this =
    inherit Screen()
    let flowContainer = new FlowContainer()
    do
        this.Add(flowContainer |> positionWidget(-400.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f))
        Percyqaz.Json.Json.Mapping.getPickler<{|data: EOPack list|}>() |> ignore
        TaskManager.AddTask("Downloading pack data",
            (fun output ->
                downloadJson("https://api.etternaonline.com/v2/packs/",
                    (fun (d: {|data: EOPack list|}) ->
                        for p in d.data do
                            flowContainer.Add(new ImportCard(p.attributes.name, p.attributes.download))
                            )) |> Async.RunSynchronously
                true), ignore, true)