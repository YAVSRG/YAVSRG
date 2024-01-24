namespace Interlude.Web.Server.Domain.Services

open Percyqaz.Common
open Interlude.Web.Server.Domain.Objects

module Users =

    let permanently_delete_user (user_id: int64) =
        Logging.Info(sprintf "Permanently deleting user with id #%i\nSay goodbye to %A" user_id (User.by_id user_id))

        Friends.on_user_deleted(user_id)

        User.delete user_id
        Logging.Info("Delete successful")