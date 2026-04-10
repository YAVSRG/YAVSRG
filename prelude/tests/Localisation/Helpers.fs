namespace Prelude.Tests.Localisation

open System.IO
open System.Text

type LocaleFakeFileSystem =
    private {
        Files: Map<string, string>
    }
    static member Create : LocaleFakeFileSystem = { Files = Map.empty }
    
    member this.Add(name: string, content: string) : LocaleFakeFileSystem= { Files = this.Files.Add(name, content) }
    
    member this.GetLocale(name: string) : Stream option =
        match this.Files.TryFind(name) with
        | Some content -> Some(new MemoryStream(Encoding.UTF8.GetBytes(content)))
        | None -> None