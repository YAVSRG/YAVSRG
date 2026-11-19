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
    let Load_OverwritesDefaultRuleset () : unit =
        let ruleset_location = Directory.CreateTempSubdirectory("rulesets").FullName
        let library = RulesetLibrary(ruleset_location)

        QUAVER_STANDARD.Ruleset.WriteToFile(Path.Combine(ruleset_location, "sc-j4.ruleset"))

        library.Load()
        let loaded = Array.ofSeq(library.ToList())

        Assert.True(library.Exists(RulesetLibrary.DEFAULT_ID))
        Assert.IsNotEmpty(loaded)
        Assert.AreEqual(1, loaded.Length)
        Assert.AreEqual(SC_J4.Hash, (snd loaded.[0]).Hash)

    [<Test>]
    let Load_SelectsDefault_WhenSelectionNotAvailable () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Select("does_not_exist")
        Assert.True(library.Selected = FileId("does_not_exist"))

        library.Load()
        Assert.True(library.Selected = FileId(RulesetLibrary.DEFAULT_ID))

    [<Test>]
    let Install_AddsNewRuleset () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let id = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.AreEqual(2, library.Count)
        Assert.True(library.Exists(id))
        Assert.AreEqual("quaver-standard", QUAVER_STANDARD.Ruleset.Filename())
        Assert.AreEqual(QUAVER_STANDARD.Ruleset.Filename(), id)

        let id2 = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.AreEqual(3, library.Count)
        Assert.True(library.Exists(id2))
        Assert.AreNotEqual(id, id2)
        Assert.True(id2.StartsWith(id))

    [<Test>]
    let Install_FromPath_AddsNewRuleset () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let external_path = Path.GetTempFileName()
        QUAVER_STANDARD.Ruleset.WriteToFile(external_path)

        let result = library.Install(external_path)

        match result with
        | None -> Assert.Fail("Expected a successful install")
        | Some(id) ->
            Assert.AreEqual(2, library.Count)
            Assert.True(library.Exists(id))
            Assert.AreEqual("quaver-standard", QUAVER_STANDARD.Ruleset.Filename())
            Assert.AreEqual(QUAVER_STANDARD.Ruleset.Filename(), id)

        let result = library.Install(external_path)

        match result with
        | None -> Assert.Fail("Expected a successful install")
        | Some(id2) ->
            Assert.AreEqual(3, library.Count)
            Assert.True(library.Exists(id2))

    [<Test>]
    let Install_FromPath_FailsWhenFileNotFound () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let result = library.Install("does-not-exist.ruleset")

        match result with
        | None -> Assert.AreEqual(1, library.Count)
        | Some _ -> Assert.Fail()

    [<Test>]
    let Install_FromPath_FailsWhenRulesetAlreadyInFolder () : unit =
        let ruleset_location = Directory.CreateTempSubdirectory("rulesets").FullName
        let library = RulesetLibrary(ruleset_location)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let ruleset_file_path = Path.Combine(ruleset_location, "quaver-standard.ruleset")
        QUAVER_STANDARD.Ruleset.WriteToFile(ruleset_file_path)

        let result = library.Install(ruleset_file_path)

        match result with
        | None -> Assert.AreEqual(1, library.Count)
        | Some _ -> Assert.Fail()

    [<Test>]
    let Install_FromPath_FailsWhenRulesetInvalid () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let external_path = Path.GetTempFileName()

        let result = library.Install(external_path)

        match result with
        | None -> Assert.AreEqual(1, library.Count)
        | Some _ -> Assert.Fail()

        let external_path2 = Path.GetTempFileName()

        let INVALID_RULESET = { SC_J4.Ruleset with Judgements = [||] }
        INVALID_RULESET.WriteToFile(external_path2)

        let result = library.Install(external_path2)

        match result with
        | None -> Assert.AreEqual(1, library.Count)
        | Some _ -> Assert.Fail()

    [<Test>]
    let Update_UpdatesRulesetWithNameUnchanged () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        let id = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.True(library.Exists(id))
        Assert.AreEqual(2, library.Count)

        Assert.AreEqual(
            QUAVER_STANDARD.Ruleset.Description,
            library.Get(None, FileId("quaver-standard")).Ruleset.Description
        )

        let updated_ruleset =
            { QUAVER_STANDARD.Ruleset with Description = "Updated description" }

        Assert.AreEqual(Some(id), library.Update(id, updated_ruleset))
        Assert.True(library.Exists(id))
        Assert.AreEqual(2, library.Count)

        Assert.AreNotEqual(
            QUAVER_STANDARD.Ruleset.Description,
            library.Get(None, FileId("quaver-standard")).Ruleset.Description
        )

    [<Test>]
    let Update_UpdatesRulesetWithNameAndFileChanged () : unit =
        let ruleset_location = Directory.CreateTempSubdirectory("rulesets").FullName
        let library = RulesetLibrary(ruleset_location)

        let file_system_state () =
            Directory.GetFiles(ruleset_location) |> Array.map Path.GetFileNameWithoutExtension |> Array.sort

        library.Load()
        Assert.AreEqual(1, library.Count)

        let id = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.True(library.Exists(id))
        Assert.AreEqual(2, library.Count)

        Assert.AreEqual(
            QUAVER_STANDARD.Ruleset.Description,
            library.Get(None, FileId("quaver-standard")).Ruleset.Description
        )

        Assert.AreEqual([| "quaver-standard"; "sc-j4" |], file_system_state())

        Assert.AreEqual(Some("wife3-j4"), library.Update(id, WIFE3_J4.Ruleset))
        Assert.False(library.Exists(id))
        Assert.AreEqual(2, library.Count)
        Assert.AreEqual(WIFE3_J4.Ruleset.Description, library.Get(None, FileId("wife3-j4")).Ruleset.Description)
        Assert.AreEqual([| "sc-j4"; "wife3-j4" |], file_system_state())

    [<Test>]
    let Update_ReturnsFalse_WhenEditingDefault () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        Assert.AreEqual(None, library.Update(RulesetLibrary.DEFAULT_ID, QUAVER_STANDARD.Ruleset))
        Assert.AreEqual(SC_J4.Ruleset, library.GetCurrent(None).Ruleset)

    [<Test>]
    let Update_ReturnsFalse_WhenIdNotFound () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()
        Assert.AreEqual(1, library.Count)

        Assert.AreEqual(None, library.Update("does_not_exist", QUAVER_STANDARD.Ruleset))
        Assert.AreEqual(SC_J4.Ruleset, library.GetCurrent(None).Ruleset)

    [<Test>]
    let OnChanged_OnlyFiresAfterLoad () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Select("my_id")
        Assert.False(event_fired)

        library.Select(RulesetLibrary.DEFAULT_ID)
        Assert.False(event_fired)

        library.Load()
        Assert.False(event_fired)

        library.Select(OsuAutomatic(8.0f))
        Assert.True(event_fired)

        event_fired <- false
        library.Select("my_id")
        Assert.True(event_fired)

    [<Test>]
    let OnChanged_FiresOnLoad_WhenIdNotFound () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Select("does_not_exist")
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
        library.Select(new_id)
        Assert.True(event_fired)

        event_fired <- false
        Assert.True(library.Delete(new_id))
        Assert.True(event_fired)

    [<Test>]
    let OnChanged_FiresOnUpdate_WhenUpdatedWasSelected () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        let mutable event_fired = false
        library.OnChanged.Add(fun () -> event_fired <- true)

        library.Load()
        Assert.False(event_fired)

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        library.Select(new_id)
        Assert.True(event_fired)

        event_fired <- false
        Assert.True(library.Update(new_id, WIFE3_J4.Ruleset).IsSome)
        Assert.True(event_fired)

    [<Test>]
    let Delete_SelectsDefault_WhenDeletedWasSelected () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)

        library.Load()

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        library.Select(new_id)

        Assert.True(library.Delete(new_id))
        Assert.False(library.Exists(new_id))
        Assert.True(library.Selected = FileId(RulesetLibrary.DEFAULT_ID))

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
            copy_of_library.Count

        library.Load()

        let new_id = library.Install(QUAVER_STANDARD.Ruleset)
        Assert.AreEqual(2, library.Count)
        Assert.AreEqual(2, reloaded_list_from_folder())

        Assert.True(library.Delete(new_id))
        Assert.AreEqual(1, library.Count)
        Assert.AreEqual(1, reloaded_list_from_folder())
