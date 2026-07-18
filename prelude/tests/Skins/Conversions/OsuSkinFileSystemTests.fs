namespace Prelude.Tests.Skins.Conversions

open System.IO
open System.IO.Compression
open NUnit.Framework
open Prelude
open Prelude.Tests.Helpers
open Prelude.Skins.Conversions.Osu

module OsuSkinFileSystemTests =

    let ONEPIXELIMAGE = new Bitmap(1, 1)

    [<Test>]
    let Empty () =
        let fs = VirtualOsuSkinFileSystem()
        Assert.True(fs.SearchForTexture("note-1H").ToOption().IsNone)
        Assert.True(fs.SearchForAnimation("note-1H").ToOption().IsNone)
        
    [<Test>]
    let Texture_Prefer2xOver1x () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
                
        let texture = fs.SearchForTexture("note-1H")
        ignore(texture.Load(fs))
        Assert.True(texture.ToOption().IsSome)
        Assert.AreEqual("note-1h@2x.png", fs.LastAccess())
        
    [<Test>]
    let Animation_Prefer2xOver1x () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        ignore(animation.Load(fs))
        Assert.True(animation.ToOption().IsSome)
        Assert.AreEqual("note-1h@2x.png", fs.LastAccess())
        
    [<Test>]
    let Texture_IgnoresFrames () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let texture = fs.SearchForTexture("note-1H")
        ignore(texture.Load(fs))
        Assert.AreEqual(Some "note-1h@2x.png", texture.ToOption())
        Assert.AreEqual("note-1h@2x.png", fs.LastAccess())
        
    [<Test>]
    let Animation_UsesFrames () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        ignore(animation.Load(fs))
        Assert.AreEqual(Some ["note-1h-0.png"], animation.ToOption())
        Assert.AreEqual("note-1h-0.png", fs.LastAccess())
        
    [<Test>]
    let Animation_UsesManyFramesPreferring2xOver1x () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .AddImage("note-1H-1.png", ONEPIXELIMAGE)
                .AddImage("note-1H-2.png", ONEPIXELIMAGE)
                .AddImage("note-1H-2@2x.png", ONEPIXELIMAGE)
                
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        let loaded = animation.Load(fs)
        Assert.AreEqual(Some ["note-1h-0@2x.png"; "note-1h-1.png"; "note-1h-2@2x.png"], animation.ToOption())
        Assert.AreEqual("note-1h-2@2x.png", fs.LastAccess())
        Assert.AreEqual([true; false; true], List.map _.Is2x loaded)
        
    [<Test>]
    let Animation_FallbackForUnloadableFrame () =
        let INVALID_IMAGE = new MemoryStream()
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .Add("note-1H-1.png", INVALID_IMAGE)
                .AddImage("note-1H-2.png", ONEPIXELIMAGE)
                .AddImage("note-1H-2@2x.png", ONEPIXELIMAGE)
                
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        let loaded = animation.Load(fs)
        Assert.AreEqual(Some ["note-1h-0@2x.png"; "note-1h-1.png"; "note-1h-2@2x.png"], animation.ToOption())
        Assert.AreEqual("note-1h-2@2x.png", fs.LastAccess())
        Assert.AreEqual([true; true; true], List.map _.Is2x loaded)
        
    [<Test>]
    let Animation_EndsAtMissingFrame () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .AddImage("note-1H-1.png", ONEPIXELIMAGE)
                .AddImage("note-1H-3.png", ONEPIXELIMAGE)
                
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        let loaded = animation.Load(fs)
        Assert.AreEqual(Some ["note-1h-0.png"; "note-1h-1.png"], animation.ToOption())
        Assert.AreEqual("note-1h-1.png", fs.LastAccess())
        Assert.AreEqual([false; false], List.map _.Is2x loaded)
        
    [<Test>]
    let Animation_FirstFrame () =
        let fs =
            VirtualOsuSkinFileSystem()
                .AddImage("note-1H-0.png", ONEPIXELIMAGE)
                .AddImage("note-1H-1.png", ONEPIXELIMAGE)
                .AddImage("note-1H-3.png", ONEPIXELIMAGE)
                
                .AddImage("note-1H@2x.png", ONEPIXELIMAGE)
                .AddImage("note-1H.png", ONEPIXELIMAGE)
        
        let animation = fs.SearchForAnimation("note-1H")
        let texture = animation.FirstFrame
        let loaded = texture.Load(fs)
        Assert.AreEqual(Some "note-1h-0.png", texture.ToOption())
        Assert.AreEqual("note-1h-0.png", fs.LastAccess())
        Assert.False(loaded.Is2x)
        
    [<Test>]
    let Texture_UseFallback () =
        let INVALID_IMAGE = new MemoryStream()
        let fs =
            VirtualOsuSkinFileSystem()
                .Add("note-1H.png", INVALID_IMAGE)
                
        let texture = fs.SearchForTexture("note-1H")
        let loaded = texture.Load(fs)
        Assert.True(texture.ToOption().IsSome)
        Assert.AreEqual("note-1h.png", fs.LastAccess())
        Assert.True(loaded.Is2x)
        Assert.AreEqual(64, loaded.Image.Width)
        
    [<Test>]
    let FromFolder_CorrectBehaviour() =
        let fs = OsuSkinFolderFileSystem("./Data")
        
        Assert.True(fs.Exists("skin.ini"))
        Assert.True(fs.Exists("replay.bin"))
        Assert.True(fs.Exists("osz/2089086 the living tombstone - my ordinary life (speed up ver.).osz"))
        
    [<Test(Description="May be a little slow to do all this file system stuff on the entire test data, consider making smaller later?")>]
    let FromZipArchive_CorrectBehaviour() =
        let ms = new MemoryStream()
        ZipFile.CreateFromDirectory("./Data", ms)
        let zip = new ZipArchive(ms, ZipArchiveMode.Read)
        let fs = OsuSkinZipFileSystem(zip)
        
        Assert.True(fs.Exists("skin.ini"))
        Assert.True(fs.Exists("replay.bin"))
        Assert.True(fs.Exists("osz/2089086 the living tombstone - my ordinary life (speed up ver.).osz"))