using System;
using System.Collections.Generic;
using System.Reflection;
using System.IO;
using System.Text.RegularExpressions;
using ManagedBass;
using OpenTK.Graphics.OpenGL;
using Prelude.Utilities;
using Interlude.Graphics;

namespace Interlude.Options.Themes
{
    public class ThemeManager
    {
        public static readonly string AssetsDir = Path.Combine(Game.WorkingDirectory, "Data", "Assets");

        public List<string> AvailableThemes { get; private set; }

        public List<Theme> LoadedThemes { get; private set; }

        public Dictionary<string, string> AssetsList { get; private set; }
        public Dictionary<string, NoteSkinMetadata> NoteSkins { get; private set; }

        protected Dictionary<string, int> Sounds;
        public TextureAtlas TextureAtlas;

        public ThemeManager()
        {
            DetectAvailableThemes();
            AssetsList = new Dictionary<string, string>();
            foreach (string line in IO.ResourceGetter.LoadText("Interlude.Resources.Assets.Assets.txt"))
            {
                var split = line.Split('|');
                AssetsList.Add(split[0], split[1]);
            }
            Load();
        }

        public void DetectAvailableThemes()
        {
            AvailableThemes = new List<string>();
            foreach (string t in Directory.EnumerateDirectories(AssetsDir))
            {
                AvailableThemes.Add(Path.GetFileName(t));
            }
        }

        public NoteSkinMetadata GetCurrentNoteSkin()
        {
            if (NoteSkins.ContainsKey(Game.Options.Profile.NoteSkin))
            {
                return NoteSkins[Game.Options.Profile.NoteSkin];
            }
            return NoteSkins["default"];
        }

        public WidgetPositionData GetUIConfig(string id)
        {
            for (int i = LoadedThemes.Count - 1; i >= 0; i--)
            {
                if (LoadedThemes[i].UIConfig.ContainsKey(id))
                {
                    return LoadedThemes[i].UIConfig[id];
                }
            }
            LoadedThemes[LoadedThemes.Count - 1].UIConfig[id] = new WidgetPositionData();
            return LoadedThemes[LoadedThemes.Count - 1].UIConfig[id];
        }

        public void Load()
        {
            Sounds = new Dictionary<string, int>();
            NoteSkins = new Dictionary<string, NoteSkinMetadata>();
            LoadedThemes = new List<Theme>
            {
                new Theme(Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Resources.Assets.fallback.zip"))
            };
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

            BuildAtlas();
        }

        void BuildAtlas()
        {
            TextureAtlas = new TextureAtlas();
            foreach (string asset in AssetsList.Keys)
            {
                if (asset.StartsWith("skin/"))
                {
                    for (int i = LoadedThemes.Count - 1; i >= 0; i--)
                    {
                        if (LoadedThemes[i].NoteSkins.ContainsKey(Game.Options.Profile.NoteSkin))
                        {
                            try
                            {
                                TextureAtlas.AddTexture(LoadedThemes[i].GetNoteSkinTexture(Game.Options.Profile.NoteSkin, asset.Substring(5)));
                                //Logging.Log("Loaded noteskin texture: " + asset, "", Logging.LogType.Debug);
                            }
                            catch
                            {
                                try
                                {
                                    //Logging.Log("Using fallback noteskin texture: "+asset, "", Logging.LogType.Debug);
                                    TextureAtlas.AddTexture(LoadedThemes[0].GetNoteSkinTexture("default", asset.Substring(5)));
                                }
                                catch
                                {
                                    Logging.Log("Error in fallback gameplay assets!", "", Logging.LogType.Warning);
                                }
                            }
                            break;
                        }
                    }
                }
                else
                {
                    for (int i = LoadedThemes.Count - 1; i >= 0; i--)
                    {

                        try
                        {
                            TextureAtlas.AddTexture(LoadedThemes[i].GetTexture(asset));
                            //Logging.Log("Loaded texture: " + asset, "", Logging.LogType.Debug);
                            break;
                        }
                        catch
                        {
                            continue;
                        }
                    }
                }
            }
            TextureAtlas.Build(false);
        }

        public Sprite GetTexture(string name)
        {
            return TextureAtlas[name];
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
            //todo: reload sounds, fonts
            TextureAtlas.Dispose();
            foreach (Theme theme in LoadedThemes)
            {
                theme.Save();
            }
        }

        public void CreateNewTheme(string name)
        {
            LoadedThemes[0].CopyTo(Path.Combine(AssetsDir, new Regex("[^a-zA-Z0-9_-]").Replace(name, "")));
        }
    }
}
