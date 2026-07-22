namespace Prelude.Tests.Shared

open NUnit.Framework
open Prelude
open Prelude.Tests.Helpers

module LocalisationTests =

    [<Test>]
    let Load_BasicFile() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#root\nkey=value1\nkey.two = value\\n2")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok data ->
            Assert.AreEqual(2, data.Count)
            Assert.AreEqual("value1", data.["key"])
            Assert.AreEqual("value\n2", data.["key.two"])
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let Load_InheritedFile() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#root\nkey=value1\nkey.two=value2\nkey.three=value3")
                .Add("de_DE", "#inherit en_GB\nkey=value eins\nkey.two=value zwei")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok data ->
            Assert.AreEqual(3, data.Count)
            Assert.AreEqual("value1", data.["key"])
            Assert.AreEqual("value2", data.["key.two"])
            Assert.AreEqual("value3", data.["key.three"])
        | Error reason -> Assert.Fail(reason)

        match Localisation.load("de_DE", files.GetLocale) with
        | Ok data ->
            Assert.AreEqual(3, data.Count)
            Assert.AreEqual("value eins", data.["key"])
            Assert.AreEqual("value zwei", data.["key.two"])
            Assert.AreEqual("value3", data.["key.three"])
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let Load_Invalid_BadHeader() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#bad_header\nkey=value1\nkey.two=value2")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Load_Invalid_BadEntry() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#bad_header\nkey\nkey.two=value2")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Load_Invalid_CircularReference() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#inherit de_DE\nkey=value1\nkey.two=value2")
                .Add("de_DE", "#inherit en_GB\nkey=value1\nkey.two=value2")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Load_Invalid_ReferenceNotFound() =
        let files =
            VirtualLocaleFileSystem
                .Create
                .Add("en_GB", "#inherit de_DE\nkey=value1\nkey.two=value2")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Load_Invalid_EntryPointNotFound() =
        let files = VirtualLocaleFileSystem.Create

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Load_Invalid_Empty() =
        let files = VirtualLocaleFileSystem.Create.Add("en_GB", "")

        match Localisation.load("en_GB", files.GetLocale) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)