namespace Interlude.UI.Screens.Import

open System
open System.IO
open Percyqaz.Json
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect

[<Json.AutoCodec>]
type BeatmapData =
    {
        difficulty_cs: float //key count
        difficulty: float
        bpm: int
        play_length: int
        mapper: string
        artist: string
        title: string
        beatmapset_id: int
        beatmap_status: int
    }
    
[<Json.AutoCodec>]
type BeatmapSearch =
    {
        result_count: int
        beatmaps: ResizeArray<BeatmapData>
    }

type private BeatmapImportCard(data: BeatmapData) as this =
    inherit Widget()
            
    let mutable downloaded = false
    let download() =
        let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".osz")
        Notification.add (Localisation.localiseWith [data.title] "notification.download.song", NotificationType.Task)
        BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.title)
            (BackgroundTask.Chain
                [
                    downloadFile(sprintf "http://beatconnect.io/b/%i/" data.beatmapset_id, target)
                    (Library.Imports.autoConvert target
                    |> BackgroundTask.Callback( fun b -> 
                        LevelSelect.refresh <- LevelSelect.refresh || b
                        Notification.add (Localisation.localiseWith [data.title] "notification.install.song", NotificationType.Task)
                        File.Delete target ))
                ]) |> ignore
        downloaded <- true
    do
        let c =
            match data.beatmap_status with
            | 1 -> Color.Aqua //ranked
            | 3 -> Color.Lime //qualified
            | 4 -> Color.HotPink //loved
            | 0 -> Color.LightGoldenrodYellow //pending
            | -1 -> Color.White //wip
            | -2 //graveyard
            | _ -> Color.Gray

        this.Position( Position.SliceTop 80.0f )
        |-+ Frame(Color.FromArgb(120, c), Color.FromArgb(200, c))
        |-+ TextBox(K (data.artist + " - " + data.title), K (Color.White, Color.Black), 0.0f)
            .Position { Left = 0.0f %+ 5.0f; Top = Position.min; Right = 1.0f %- 400.0f; Bottom = 1.0f %- 30.0f }
        |-+ TextBox(K ("Created by " + data.mapper), K (Color.White, Color.Black), 0.0f)
            .Position { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 40.0f; Right = Position.max; Bottom = Position.max }
        |-+ TextBox(K (sprintf "%.2f*   %iBPM   %iK" data.difficulty data.bpm (int data.difficulty_cs)), K (Color.White, Color.Black), 1.0f)
            .Position { Left = Position.min; Top = 0.0f %+ 20.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %- 20.0f }
        |=+ Clickable((fun () -> if not downloaded then download()), ignore)

type private SearchContainerLoader(t) as this =
    inherit Widget()
    let t = t this
    let mutable task = None
    do this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 30.0f, 0.0f)

    //loader is only drawn if it is visible on screen
    override this.Draw() =
        base.Draw()
        //todo: improved loading indicator here
        Text.drawFill(Content.font, "Loading...", this.Bounds, Color.White, 0.5f)
        if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" (t |> BackgroundTask.Callback(fun _ -> if this.Parent.IsSome then this.Destroy()))

module private Beatmap =
    
    let rec search (filter: Filter) (page: int) : FlowContainer -> SearchContainerLoader -> StatusTask =
        let mutable s = "https://osusearch.com/api/search?modes=Mania&key="
        let mutable invalid = false
        let mutable title = ""
        List.iter(
            function
            | Impossible -> invalid <- true
            | String "-p" -> s <- s + "&premium_mappers=true"
            | String s -> match title with "" -> title <- s | t -> title <- t + " " + s
            | Criterion ("k", n)
            | Criterion ("key", n)
            | Criterion ("keys", n) -> match Int32.TryParse n with (true, i) -> s <- s + sprintf "&cs=(%i.0, %i.0)" i i | _ -> ()
            | Criterion ("m", m)
            | Criterion ("c", m)
            | Criterion ("creator", m)
            | Criterion ("mapper", m) -> s <- s + "&mapper=" + m
            | _ -> ()
        ) filter
        s <- s + "&title=" + Uri.EscapeDataString title
        s <- s + "&offset=" + page.ToString()
        fun (flowContainer: FlowContainer) (loader: SearchContainerLoader) output ->
            let callback(d: BeatmapSearch) =
                if loader.Parent.IsSome then
                    flowContainer.Synchronized(
                        fun () -> 
                            for p in d.beatmaps do flowContainer.Add(BeatmapImportCard p)
                            if d.result_count < 0 || d.result_count > d.beatmaps.Count then
                                new SearchContainerLoader(search filter (page + 1) flowContainer)
                                |> flowContainer.Add
                    )
            downloadJson(s, callback)