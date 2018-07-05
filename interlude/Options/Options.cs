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
        public static string[] Skins;
        public static General General;

        public Profile Profile;
        public Theme Theme;

        public Options()
        {
            Profile = new Profile();
            Theme = new Theme();
            try
            {
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
                Utilities.Logging.Log("Couldn't switch to selected profile", Utilities.Logging.LogType.Error);
            }
        }

        public static string ProfilePath
        {
            get
            {
                return Path.Combine(General.WorkingDirectory, "Data", "Profiles");
            }
        }

        public static void EnsureFoldersExist()
        {
            Directory.CreateDirectory(Path.Combine(General.WorkingDirectory, "Songs"));
            Directory.CreateDirectory(Path.Combine(General.WorkingDirectory, "Imports"));
            Directory.CreateDirectory(Path.Combine(General.WorkingDirectory, "Data", "Profiles"));
            Directory.CreateDirectory(Path.Combine(General.WorkingDirectory, "Data", "Assets", "_Fallback"));
        }

        public static void Init()
        {
            Profiles = new List<Profile>();
            General = new General();
            try
            {
                General = Utils.LoadObject<General>("Options.json");
            }
            catch
            {
                Utilities.Logging.Log("Couldn't load settings file", Utilities.Logging.LogType.Error);
            }
            EnsureFoldersExist();
            foreach (string path in Directory.GetFiles(ProfilePath))
            {
                if (Path.GetExtension(path).ToLower() == ".json")
                {
                    try
                    {
                        Profile p = Utils.LoadObject<Profile>(path);
                        p.ProfilePath = Path.GetFileName(path);
                        Profiles.Add(p);
                    }
                    catch
                    {
                        Utilities.Logging.Log("Could not load profile!\n" + path, Utilities.Logging.LogType.Error);
                    }
                }
            }
            string[] s = Directory.GetDirectories(Path.Combine(General.WorkingDirectory, "Data", "Assets"));
            Skins = new string[s.Length];
            for (int i = 0; i < s.Length; i++)
            {
                Skins[i] = Path.GetFileName(s[i]);
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
            Utils.SaveObject(General, "Options.json");
        }
    }
}
