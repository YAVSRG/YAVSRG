using System;
using System.Collections.Generic;
using System.IO;
using Interlude.IO;

namespace Interlude.Options
{
    public class Options
    {
        public static List<Profile> Profiles;
        public static string[] Skins;
        public static General general;

        public Profile Profile;
        public ThemeData Theme;
        public General General
        {
            get { return general; }
        }

        public Options()
        {
            Profile = new Profile();
            Theme = new ThemeData();
            try
            {
                foreach (Profile p in Profiles) //linear search cause i'm lazy, this runs once and you're not gonna have more than like 20 profiles ever
                {
                    if (p.ProfilePath == general.CurrentProfile)
                    {
                        ChangeProfile(p);
                    }
                }
            }
            catch (Exception e)
            {
                //log that settings have been reset due to load failure
                Utilities.Logging.Log("Couldn't switch to selected profile", e.ToString(), Utilities.Logging.LogType.Error);
            }
        }

        public static string ProfilePath
        {
            get
            {
                return Path.Combine(general.WorkingDirectory, "Data", "Profiles");
            }
        }

        public static void EnsureFoldersExist()
        {
            Directory.CreateDirectory(Path.Combine(general.WorkingDirectory, "Songs"));
            Directory.CreateDirectory(Path.Combine(general.WorkingDirectory, "Imports"));
            Directory.CreateDirectory(Path.Combine(general.WorkingDirectory, "Data", "Profiles"));
            Directory.CreateDirectory(Path.Combine(general.WorkingDirectory, "Data", "Assets", "_Fallback"));
        }

        public static void Init()
        {
            Profiles = new List<Profile>();
            if (File.Exists("Options.json"))
            {
                general = Utils.LoadObject<General>("Options.json");
            }
            else
            {
                general = new General();
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
                    catch (Exception e)
                    {
                        Utilities.Logging.Log("Could not load profile from " + path, e.ToString(), Utilities.Logging.LogType.Error);
                    }
                }
            }
            string[] s = Directory.GetDirectories(Path.Combine(general.WorkingDirectory, "Data", "Assets"));
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
            general.CurrentProfile = p.ProfilePath;
            Content.ClearStore();
            Theme = Content.LoadThemeData(p.Skin);
        }

        public void SaveProfile(Profile p)
        {
            Utils.SaveObject(p, Path.Combine(ProfilePath, p.ProfilePath));
        }

        public void Save()
        {
            SaveProfile(Profile);
            Utils.SaveObject(general, "Options.json");
        }
    }
}
