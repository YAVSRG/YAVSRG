namespace Interlude.Web.Server

open System

module Timestamp =

    let now() = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()