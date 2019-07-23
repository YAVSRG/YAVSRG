using System;
using System.Collections.Generic;
using System.Reflection;
using System.IO;
using ManagedBass;
using OpenTK.Graphics.OpenGL;
using Prelude.Utilities;
using Interlude.Graphics;

namespace Interlude.Options.Themes
{
    public class ThemeManager
    {
        public static readonly string AssetsDir = Path.Combine(Game.WorkingDirectory, "Data", "Assets");

        public List<string> AvailableThemes;

        public List<Theme> LoadedThemes;

        public Dictionary<string, NoteSkin> NoteSkins;
        protected Dictionary<string, Sprite> Textures;
        protected Dictionary<string, int> Sounds;

        public ThemeManager()
        {
            AvailableThemes = new List<string>();
            foreach (string t in Directory.EnumerateDirectories(AssetsDir))
            {
                AvailableThemes.Add(Path.GetFileName(t));
            }
            Load();
        }

        public void Load()
        {
            Textures = new Dictionary<string, Sprite>();
            Sounds = new Dictionary<string, int>();
            NoteSkins = new Dictionary<string, NoteSkin>();
            LoadedThemes = new List<Theme>();
            LoadedThemes.Add(new Theme(Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources.Assets.fallback.zip")));
            foreach (string t in Game.Options.Profile.SelectedThemes)
            {
                if (AvailableThemes.Contains(t))
                {
                    try
                    {
                        LoadedThemes.Add(new Theme(Path.Combine(AssetsDir, t)));
                    }
                    catch (Exception e)
                    {
                        Logging.Log("Failed to load theme: " + t, e.ToString(), Logging.LogType.Error);
                    }
                }
            }
            foreach (Theme theme in LoadedThemes)
            {
                foreach (string noteskin in theme.NoteSkins.Keys)
                {
                    NoteSkins[noteskin] = theme.NoteSkins[noteskin];
                }
            }
        }

        public Sprite GetTexture(string name)
        {
            if (!Textures.ContainsKey(name))
            {
                Sprite t = default;
                for (int i = LoadedThemes.Count - 1; i >= 0; i--)
                {

                    try
                    {
                        t = LoadedThemes[i].GetTexture(name);
                        break;
                    }
                    catch
                    {
                        continue;
                    }
                }
                Textures[name] = t;
            }
            return Textures[name];
        }

        public Sprite GetNoteSkinTexture(string name)
        {
            if (!Textures.ContainsKey(name))
            {
                Sprite t = default;
                for (int i = LoadedThemes.Count - 1; i >= 0; i--)
                {
                    if (LoadedThemes[i].NoteSkins.ContainsKey(Game.Options.Profile.NoteSkin))
                    {
                        try
                        {
                            t = LoadedThemes[i].GetNoteSkinTexture(Game.Options.Profile.NoteSkin, name);
                        }
                        catch
                        {
                            try
                            {
                                t = LoadedThemes[0].GetNoteSkinTexture("default", name);
                            }
                            catch
                            {
                                Logging.Log("Error in fallback gameplay assets!", "", Logging.LogType.Warning);
                            }
                        }
                        break;
                    }
                }
                Textures[name] = t;
            }
            return Textures[name];
        }

        public int GetSound(string name)
        {
            if (!Sounds.ContainsKey(name))
            {
                byte[] sound = new byte[0];
                for (int i = LoadedThemes.Count - 1; i >= 0; i--)
                {

                    try
                    {
                        sound = LoadedThemes[i].GetSound(name);
                        break;
                    }
                    catch
                    {
                        continue;
                    }
                }
                Sounds[name] = Bass.SampleLoad(sound, 0, sound.Length, 65535, BassFlags.AutoFree);
            }
            return Sounds[name];
        }

        public void Unload()
        {
            foreach (string id in Textures.Keys)
            {
                GL.DeleteTexture(Textures[id].ID);
            }
        }
    }
}
