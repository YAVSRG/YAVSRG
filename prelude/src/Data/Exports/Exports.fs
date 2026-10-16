namespace Prelude.Data.Exports

open System.IO
open System.IO.Compression
open Prelude.Charts
open Prelude.Formats.Osu
open Prelude.Data.Library

module Exports =

    /// Creates an .osz representation of the chart in the given folder `export_folder`
    /// If successful, the exported beatmap data + the filename of the .osz are returned
    let create_osz (options: OsuExportOptions) (chart: Chart) (chart_meta: ChartMeta) (export_folder: string) : Result<Beatmap * string, exn> =
        try
            let beatmap = OsuExport.convert options chart chart_meta

            let file_name = beatmap.Filename()
            let archive_file_name = file_name.Replace(".osu", ".osz")
            let archive_path = Path.Combine(export_folder, archive_file_name)

            use fs = File.Open(archive_path, FileMode.Create)
            use archive = new ZipArchive(fs, ZipArchiveMode.Create, false)

            do
                let osu_file_entry = archive.CreateEntry(file_name)
                use osu_file_stream = osu_file_entry.Open()
                beatmap.WriteToStream (osu_file_stream, false)

            do
                match chart_meta.Background.Path with
                | Some bg_path when File.Exists(bg_path) ->
                    use fs = File.Open(bg_path, FileMode.Open)
                    let bg_file_entry = archive.CreateEntry(beatmap.Events |> Seq.pick (function Background(bg, _, _) -> Some bg | _ -> None))
                    use bg_file_stream = bg_file_entry.Open()
                    fs.CopyTo(bg_file_stream)
                | _ -> ()

            do
                match chart_meta.Audio.Path with
                | Some audio_path when File.Exists(audio_path) ->
                    use fs =
                        File.Open(audio_path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite)

                    let audio_file_entry = archive.CreateEntry(beatmap.General.AudioFilename)
                    use audio_file_stream = audio_file_entry.Open()
                    fs.CopyTo(audio_file_stream)
                | _ -> ()
            Ok (beatmap, archive_file_name)
        with err ->
            Error err