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
        public General General;

        public Options()
        {
            Profile = new Profile();
            Theme = new Theme();
            General = new General();
            try
            {
                General = Utils.LoadObject<General>(Path.Combine(Content.WorkingDirectory, "Data", "Options.json"));
                foreach (Profile p in Profiles) //linear search cause i'm lazy, this runs once and you're not gonna have more than like 20 profiles ever
                {
                    if (p.ProfilePath == General.CurrentProfile)
                    {
                        ChangeProfile(p);
                    }
                }
            }
            catch
            {
                //log that settings have been reset due to load failure
            }
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
                        p.ProfilePath = Path.GetFileName(s);
                        Profiles.Add(p);
                    }
                    catch
                    {
                        //:( log that you couldn't load the profile
                    }
                }
            }
        }

        public void ChangeProfile(Profile p)
        {
            //remember to save the old one
            SaveProfile(Profile);
            Profile = p;
            General.CurrentProfile = p.ProfilePath;
            Theme = Content.LoadThemeData(p.Skin);
        }

        public void SaveProfile(Profile p)
        {
            Utils.SaveObject(p, Path.Combine(ProfilePath, p.ProfilePath));
        }

        public void Save()
        {
            SaveProfile(Profile);
            Utils.SaveObject(General, Path.Combine(Content.WorkingDirectory, "Data", "Options.json"));
        }
    }
}
