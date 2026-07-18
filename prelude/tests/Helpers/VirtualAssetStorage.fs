namespace Prelude.Tests.Helpers

open Prelude.Data.Library

type VirtualAssetStorage(path: string) =
    inherit AssetStorage()

    [<Literal>]
    static let VIRTUAL_HASH =
        "00facade00facade00facade00facade00facade00facade00facade00facade"

    override this.Add(file_path: string) : string =
        printfn "virtual assets: attempted to hash and add path '%s'" file_path
        VIRTUAL_HASH

    override this.Remove(hash: string) : int64 =
        ignore hash
        -1L

    override this.Contains(hash: string) : bool = hash = VIRTUAL_HASH

    override this.GetPath(hash: string) : string =
        ignore hash
        path

    override this.Enumerate() : string seq = [ VIRTUAL_HASH ]
