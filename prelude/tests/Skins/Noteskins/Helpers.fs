namespace Prelude.Tests.Skins.Noteskins

open System.IO
open System.IO.Compression
open SixLabors.ImageSharp
open Prelude
open Prelude.Skins.Noteskins

module Helpers =

    type InMemoryNoteskinBuilder(config: NoteskinConfig) =
        let memory_stream = new MemoryStream()
        let zip_archive = new ZipArchive(memory_stream, ZipArchiveMode.Create, true)

        do
            let config_file = zip_archive.CreateEntry("noteskin.json")
            use stream = config_file.Open()
            JSON.ToStream stream config

        member this.AddJsonFile(path: string, data: 'T) : InMemoryNoteskinBuilder =
            let entry = zip_archive.CreateEntry(path)
            use stream = entry.Open()
            JSON.ToStream stream data
            this

        member this.AddImageFile(path: string, image: Bitmap) : InMemoryNoteskinBuilder =
            let entry = zip_archive.CreateEntry(path)
            use stream = entry.Open()
            image.SaveAsPng stream
            this

        member this.Build() : Noteskin =
            zip_archive.Dispose()
            memory_stream.Position <- 0

            Noteskin.FromZipStream(memory_stream)