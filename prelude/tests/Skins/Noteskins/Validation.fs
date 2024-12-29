namespace Prelude.Tests.Skins.Noteskins

open NUnit.Framework
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins
open Helpers

module Validation =

    let ONEPIXELIMAGE = new Bitmap(1, 1)

    [<Test>]
    let MissingTextures () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(2, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "note" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for missing 'note' texture")
        | _ -> ()

        match
            Array.tryFind
                (function
                | ValidationError { Element = "receptor" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for missing 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithRequiredTextures_Grid () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[1x1].png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(0, validation_results.Length)

    [<Test>]
    let WithRequiredTextures_Loose () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("receptor-0-0.png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(0, validation_results.Length)

    [<Test>]
    let WithRequiredTextures_Loose_Multiple () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("note-0-1.png", ONEPIXELIMAGE)
                .AddImageFile("note-1-0.png", ONEPIXELIMAGE)
                .AddImageFile("note-1-1.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("receptor-0-0.png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(0, validation_results.Length)

    [<Test>]
    let WithRequiredTextures_Loose_OneMissing () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("note-0-1.png", ONEPIXELIMAGE)
                .AddImageFile("note-1-1.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("receptor-0-0.png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "note" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for missing loose 'note' texture")
        | _ -> ()

    [<Test>]
    let WithExtraTextures () =
        let noteskin =
            InMemoryNoteskinBuilder(
                { NoteskinConfig.Default with
                    EnableColumnLight = false
                }
            )
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("noteexplosion[1x1].png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(2, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationWarning { Element = "noteexplosion" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected a warning message for unnecessary 'noteexplosion' texture")
        | _ -> ()

        match
            Array.tryFind
                (function
                | ValidationWarning { Element = "receptorlighting" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected a warning message for unnecessary 'receptorlighting' texture")
        | _ -> ()

    [<Test>]
    [<Ignore("Currently the user can mix textures this way and not get an error")>] // todo: validate against this
    let WithMixedGridAndLooseFiles () =
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("note-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[1x1].png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "note" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for wrong mode on 'note' texture")
        | _ -> ()

    [<Test>]
    let WithMixedGridFiles () =
        use receptor_grid_bmp = new Bitmap(16, 3)
        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[16x3].png", receptor_grid_bmp)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "receptor" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for wrong mode on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithTooManyRows () =
        use receptor_grid_bmp = new Bitmap(16, 3)

        let noteskin =
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[16x21].png", receptor_grid_bmp)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "receptor" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for row count on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithWrongGridImageDimensions () =
        use receptor_grid_bmp = new Bitmap(16, 24)

        let noteskin =
            InMemoryNoteskinBuilder(
                { NoteskinConfig.Default with
                    ReceptorStyle = ReceptorStyle.Receptors
                }
            )
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[16x2].png", receptor_grid_bmp)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "receptor" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for image size on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let SuggestRowsAndColumnsBackwards () =
        use receptor_grid_bmp = new Bitmap(200, 700)

        let noteskin =
            InMemoryNoteskinBuilder(
                { NoteskinConfig.Default with
                    ReceptorStyle = ReceptorStyle.Receptors
                }
            )
                .AddImageFile("note[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[7x2].png", receptor_grid_bmp)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "receptor" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for image size on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let SuggestRowsAndColumnsBackwards_Square () =
        use note_grid_bmp = new Bitmap(64, 16)

        let noteskin =
            InMemoryNoteskinBuilder(
                { NoteskinConfig.Default with
                    ReceptorStyle = ReceptorStyle.Receptors
                }
            )
                .AddImageFile("note[1x4].png", note_grid_bmp)
                .AddImageFile("holdbody[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdhead[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("holdtail[1x1].png", ONEPIXELIMAGE)
                .AddImageFile("receptor[1x1].png", ONEPIXELIMAGE)
                .Build()

        let validation_results = noteskin.Validate() |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(1, validation_results.Length)

        match
            Array.tryFind
                (function
                | ValidationError { Element = "note" } -> true
                | _ -> false)
                validation_results
        with
        | None -> Assert.Fail("Expected an error message for image size on 'note' texture")
        | _ -> ()