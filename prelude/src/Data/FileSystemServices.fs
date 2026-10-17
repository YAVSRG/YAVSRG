namespace Prelude.Data

open System.IO
open Percyqaz.Common

module FileSystemServices =

    let delete_folder =
        { new Async.Queue<string, bool>() with
            override this.Handle(path: string) =
                async {
                    if Directory.Exists(path) then
                        Logging.Debug "Deleting folder '%s'" path
                        let mutable remaining_retries = 3
                        while remaining_retries > 0 do
                            try
                                Directory.Delete(path, true)
                                remaining_retries <- -1
                            with err ->
                                remaining_retries <- remaining_retries - 1
                                Logging.Debug "Deleting folder '%s' failed, retrying in 1s: %s" path err.Message
                                System.Threading.Thread.Sleep(1000)
                        if remaining_retries = 0 then Logging.Error "Deleting folder '%s' failed, out of retries" path
                        return remaining_retries < 0
                    else
                        return true
                }
        }

    let delete_file =
        { new Async.Queue<string, bool>() with
            override this.Handle(path) =
                async {
                    Logging.Debug "Deleting file '%s'" path
                    let mutable remaining_retries = 3
                    while remaining_retries > 0 do
                        try
                            File.Delete path
                            remaining_retries <- -1
                        with err ->
                            remaining_retries <- remaining_retries - 1
                            Logging.Debug "Deleting file '%s' failed, retrying in 1s: %s" path err.Message
                            System.Threading.Thread.Sleep(1000)
                    if remaining_retries = 0 then Logging.Error "Deleting file '%s' failed, out of retries" path
                    return remaining_retries < 0
                }
        }