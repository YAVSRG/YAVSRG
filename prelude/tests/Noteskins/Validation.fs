namespace Prelude.Tests.Noteskins

open NUnit.Framework
open Prelude.Common
open Prelude.Data.Content
open Helpers

module Validation =

    let ONEPIXELIMAGE = new Bitmap(1, 1)

    [<Test>]
    let MissingTextures () =
        let noteskin = 
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()

        let validation_results =
            noteskin.Validate()
            |> Array.ofSeq

        printfn "%A" validation_results

        Assert.AreEqual(2, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "note" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for missing 'note' texture")
        | _ -> ()

        match Array.tryFind (function ValidationError { Element = "receptor" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for missing 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithRequiredTextures () =
        let noteskin = 
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddImageFile("receptor.png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
    
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq

        printfn "%A" validation_results
    
        Assert.AreEqual(0, validation_results.Length)

    [<Test>]
    let WithExtraTextures () =
        let noteskin = 
            InMemoryNoteskinBuilder({ NoteskinConfig.Default with EnableColumnLight = false })
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddImageFile("receptor.png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .AddImageFile("noteexplosion.png", ONEPIXELIMAGE)
                .Build()
        
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
    
        printfn "%A" validation_results
        
        Assert.AreEqual(2, validation_results.Length)

        match Array.tryFind (function ValidationWarning { Element = "noteexplosion" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected a warning message for unnecessary 'noteexplosion' texture")
        | _ -> ()

        match Array.tryFind (function ValidationWarning { Element = "receptorlighting" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected a warning message for unnecessary 'receptorlighting' texture")
        | _ -> ()
    
    [<Test>]
    let WithWrongNoteTextureModeLoose () =
        let noteskin = 
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddJsonFile("note.json", { Columns = 1; Rows = 1; Mode = Loose })
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddImageFile("receptor.png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
        
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
    
        printfn "%A" validation_results
        
        Assert.AreEqual(1, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "note" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for wrong mode on 'note' texture")
        | _ -> ()
        
    [<Test>]
    let WithWrongNoteTextureModeGrid () =
        let noteskin = 
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note-0-0.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddImageFile("receptor.png", ONEPIXELIMAGE)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
            
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
        
        printfn "%A" validation_results
            
        Assert.AreEqual(1, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "note" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for wrong mode on 'note' texture")
        | _ -> ()

    [<Test>]
    let WithTooManyRows () =
        use receptor_grid_bmp = new Bitmap(16, 3)
        let noteskin = 
            InMemoryNoteskinBuilder(NoteskinConfig.Default)
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddJsonFile("receptor.json", { Columns = 16; Rows = 3; Mode = Grid })
                .AddImageFile("receptor.png", receptor_grid_bmp)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
        
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
    
        printfn "%A" validation_results
        
        Assert.AreEqual(1, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "receptor" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for row count on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithWrongGridImageDimensions () =
        use receptor_grid_bmp = new Bitmap(16, 24)
        let noteskin = 
            InMemoryNoteskinBuilder({ NoteskinConfig.Default with ReceptorStyle = ReceptorStyle.Rotate })
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddJsonFile("receptor.json", { Columns = 16; Rows = 2; Mode = Grid })
                .AddImageFile("receptor.png", receptor_grid_bmp)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
        
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
    
        printfn "%A" validation_results
        
        Assert.AreEqual(1, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "receptor" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for image size on 'receptor' texture")
        | _ -> ()

    [<Test>]
    let WithRowsAndColumnsBackwards () =
        use receptor_grid_bmp = new Bitmap(200, 700)
        let noteskin = 
            InMemoryNoteskinBuilder({ NoteskinConfig.Default with ReceptorStyle = ReceptorStyle.Rotate })
                .AddImageFile("note.png", ONEPIXELIMAGE)
                .AddImageFile("holdbody.png", ONEPIXELIMAGE)
                .AddImageFile("holdhead.png", ONEPIXELIMAGE)
                .AddImageFile("holdtail.png", ONEPIXELIMAGE)
                .AddJsonFile("receptor.json", { Columns = 7; Rows = 2; Mode = Grid })
                .AddImageFile("receptor.png", receptor_grid_bmp)
                .AddImageFile("receptorlighting.png", ONEPIXELIMAGE)
                .Build()
        
        let validation_results = 
            noteskin.Validate()
            |> Array.ofSeq
    
        printfn "%A" validation_results
        
        Assert.AreEqual(1, validation_results.Length)

        match Array.tryFind (function ValidationError { Element = "receptor" } -> true | _ -> false) validation_results with
        | None -> Assert.Fail("Expected an error message for image size on 'receptor' texture")
        | _ -> ()