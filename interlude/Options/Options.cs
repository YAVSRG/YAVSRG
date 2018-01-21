using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Options
{
    class Options
    {
        public static List<Profile> Profiles;

        public Profile Profile;
        public Theme Theme;

        public Options()
        {
            Profile = new Profile();
            Theme = new Theme();
        }

        public static string ProfilePath
        {
            get
            {
                return Path.Combine(Content.WorkingDirectory, "Data", "Profiles");
            }
        }

        public static void Init()
        {
            Profiles = new List<Profile>();
            foreach (string s in Directory.GetFiles(ProfilePath))
            {
                if (Path.GetExtension(s) == ".json")
                {
                    try
                    {
                        Profile p = Utils.LoadObject<Profile>(s);
                        Profiles.Add(p);
                    }
                    catch
                    {
                        //:( log that you couldn't load the profile
                    }
                }
            }
            if (Profiles.Count > 0)
            {
                Game.Options.ChangeProfile(Profiles[0]);
            }
        }

        public void ChangeProfile(Profile p)
        {
            Profile = p;
            Theme = Content.LoadThemeData(p.Skin);
        }

        public void SaveProfile()
        {
            Utils.SaveObject(Profile, Path.Combine(ProfilePath, Profile.Name + ".json"));
        }
    }
}
