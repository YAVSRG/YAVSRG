using System;
using System.Collections.Generic;
using System.IO;
using Prelude.Utilities;
using Interlude.Options.Themes;

namespace Interlude.Options
{
    public class SettingsManager
    {
        public static List<Profile> Profiles;
        public static General general;

        public Profile Profile;
        public ThemeManager Themes;

        public General General
        {
            get { return general; }
        }

        public ThemeOptions Theme
        {
            get { return Themes.LoadedThemes[Themes.LoadedThemes.Count - 1].Config; }
        }

        public SettingsManager()
        {
            Profile = new Profile();
            try
            {
                foreach (Profile p in Profiles)
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
                Logging.Log("Couldn't switch to selected profile", e.ToString(), Logging.LogType.Error);
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
            Directory.CreateDirectory(Path.Combine(general.WorkingDirectory, "Data", "Assets"));
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
                        Logging.Log("Could not load profile from " + path, e.ToString(), Logging.LogType.Error);
                    }
                }
            }
        }

        public void ChangeProfile(Profile p)
        {
            //remember to save the old one
            SaveProfile(Profile);
            Themes?.Unload();
            Profile = p;
            general.CurrentProfile = p.ProfilePath;
            Themes?.Load();
        }

        public void SaveProfile(Profile p)
        {
            Utils.SaveObject(p, Path.Combine(ProfilePath, p.ProfilePath));
        }

        public void Save()
        {
            SaveProfile(Profile);
            Utils.SaveObject(general, "Options.json");
            Themes.Unload();
        }
    }
}
