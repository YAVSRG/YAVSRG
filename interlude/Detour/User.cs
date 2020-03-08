using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace InterludeServer
{
    class User
    {
        public string Username = "";
        public DateTime Created = DateTime.Now;
        public DateTime LastSeen = DateTime.Now;

        public static Dictionary<string, User> UserDB = Interlude.Utils.LoadObject<Dictionary<string, User>>("Users.json");
    }
}
