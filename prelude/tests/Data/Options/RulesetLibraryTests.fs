namespace Prelude.Tests.Data.Options

open System.IO
open NUnit.Framework
open Prelude.Data.Options
open Prelude.Gameplay.Rulesets

module RulesetLibraryTests =

    [<Test>]
    let Constructor_CreatesEmptyLibrary () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)
        Assert.IsEmpty(library.ToList())

    [<Test>]
    let Load_ProvidesDefaultRuleset () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        let loaded = Array.ofSeq(library.ToList())

        Assert.True(library.Exists(RulesetLibrary.DEFAULT_ID))
        Assert.IsNotEmpty(loaded)
        Assert.AreEqual(1, loaded.Length)
        Assert.AreEqual(SC_J4.Hash, (snd loaded.[0]).Hash)

    [<Test>]
    let Load_SelectsDefault_WhenSelectionNotAvailable () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Selected.Set(FileId("does_not_exist"))
        Assert.True(library.Selected.Value = FileId("does_not_exist"))

        library.Load()
        Assert.True(library.Selected.Value = FileId(RulesetLibrary.DEFAULT_ID))


    [<Test>]
    let OnChanged_OnlyFiresAfterLoad () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Selected.Set(FileId("my_id"))
        Assert.False(event_fired)

        library.Selected.Set(FileId(RulesetLibrary.DEFAULT_ID))
        Assert.False(event_fired)

        library.Load()
        Assert.False(event_fired)

        library.Selected.Set(OsuAutomatic(8.0f))
        Assert.True(event_fired)

        event_fired <- false
        library.Selected.Set(FileId("my_id"))
        Assert.True(event_fired)

    [<Test>]
    let OnChanged_FiresOnLoad_WhenIdNotFound () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Selected.Set(FileId("does_not_exist"))
        Assert.False(event_fired)

        library.Load()
        Assert.True(event_fired)

    [<Test>]
    let OnChanged_FiresOnDelete_WhenDeletedWasSelected () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Load()
        Assert.False(event_fired)

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        library.Selected.Set(FileId(new_id))
        Assert.True(event_fired)

        event_fired <- false
        Assert.True(library.Delete(new_id))
        Assert.True(event_fired)

    [<Test>]
    let Delete_SelectsDefault_WhenDeletedWasSelected () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        library.Selected.Set(FileId(new_id))

        Assert.True(library.Delete(new_id))
        Assert.False(library.Exists(new_id))
        Assert.True(library.Selected.Value = FileId(RulesetLibrary.DEFAULT_ID))

    [<Test>]
    let Delete_ReturnsFalse_WhenDeletingDefault () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()

        Assert.False(library.Delete(RulesetLibrary.DEFAULT_ID))
        Assert.True(library.Exists(RulesetLibrary.DEFAULT_ID))

    [<Test>]
    let Delete_ReturnsFalse_WhenIdNotFound () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()

        Assert.False(library.Exists("does_not_exist"))

        Assert.False(library.Delete("does_not_exist"))
        Assert.False(library.Exists("does_not_exist"))

    [<Test>]
    let Delete_DeletesFile () : unit =
        let ruleset_location = Directory.CreateTempSubdirectory("rulesets").FullName
        let library = RulesetLibrary(ruleset_location)

        let reloaded_list_from_folder () =
            let copy_of_library = RulesetLibrary(ruleset_location)
            copy_of_library.Load()
            copy_of_library.ToList()

        library.Load()

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.AreEqual(2, Seq.length(library.ToList()))
        Assert.AreEqual(2, Seq.length(reloaded_list_from_folder()))

        Assert.True(library.Delete(new_id))
        Assert.AreEqual(1, Seq.length(library.ToList()))
        Assert.AreEqual(1, Seq.length(reloaded_list_from_folder()))
