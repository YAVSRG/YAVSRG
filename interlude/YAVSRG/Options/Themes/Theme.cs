using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Drawing;
using Prelude.Utilities;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Options.Themes
{
    public class Theme
    {
        public ThemeOptions Config;

        public Dictionary<string, NoteSkin> NoteSkins;

        public Dictionary<string, WidgetPositionData> UIConfig;

        private ZipArchive zipFile;

        public string ThemePath;
        
        public Theme(string path)
        {
            ThemePath = path;
            Config = Utils.LoadObject<ThemeOptions>(GetFile("theme.json"));
            NoteSkins = new Dictionary<string, NoteSkin>();
            UIConfig = new Dictionary<string, WidgetPositionData>();
            foreach (string noteskin in Directory.EnumerateDirectories(Path.Combine(ThemePath, "NoteSkins")))
            {
                string name = Path.GetFileName(noteskin);
                try
                {
                    NoteSkins[name] = Utils.LoadObject<NoteSkin>(GetFile("NoteSkins", name, "noteskin.json"));
                }
                catch (Exception e)
                {
                    Logging.Log("Could not load noteskin: " + name, e.ToString(), Logging.LogType.Error);
                }
            }
            Directory.CreateDirectory(Path.Combine(ThemePath, "Interface"));
            foreach (string file in Directory.EnumerateFiles(Path.Combine(ThemePath, "Interface")))
            {
                if (Path.GetExtension(file).ToLower() == ".json")
                {
                    UIConfig[Path.GetFileNameWithoutExtension(file)] = Utils.LoadObject<WidgetPositionData>(file);
                }
            }
        }

        public Theme(Stream zipStream)
        {
            zipFile = new ZipArchive(zipStream, ZipArchiveMode.Read, false);
            Config = Utils.LoadObject<ThemeOptions>(GetFile("theme.json"));
            //todo: hard code more stuff as time goes on like a jackass
            UIConfig = new Dictionary<string, WidgetPositionData>() { { "gameplay", Utils.LoadObject<WidgetPositionData>(GetFile("Interface", "gameplay.json")) } };
            NoteSkins = new Dictionary<string, NoteSkin>() { { "default", Utils.LoadObject<NoteSkin>(GetFile("NoteSkins", "default", "noteskin.json")) } };
        }

        public Sprite GetTexture(string name)
        {
            Bitmap bmp; //fuck you c# look at this shit
            //how is this difficult
            using (var stream = GetFile("Textures", name + ".png"))
            {
                bmp = new Bitmap(stream);
            }
            TextureData info;
            try { info = Utils.LoadObject<TextureData>(GetFile("Textures", name + ".json")); } catch { info = new TextureData(); }
            return Content.UploadTexture(bmp, info.Columns, info.Rows, false);
        }

        public Sprite GetNoteSkinTexture(string noteskinname, string name)
        {
            Bitmap bmp;
            using (var stream = GetFile("NoteSkins", noteskinname, name + ".png"))
            {
                bmp = new Bitmap(stream);
            }
            TextureData info;
            try { info = Utils.LoadObject<TextureData>(GetFile("NoteSkins", noteskinname, name + ".json")); } catch { info = new TextureData(); }
            return Content.UploadTexture(bmp, info.Columns, info.Rows, false);
        }

        public byte[] GetSound(string name)
        {
            using (var stream = GetFile("Sounds", name + ".wav"))
            using (var ms = new MemoryStream())
            {
                stream.CopyTo(ms);
                return ms.ToArray();
            }
        }

        public Stream GetFile(params string[] path)
        {
            string p = Path.Combine(path);
            if (zipFile != null)
            {
                return zipFile.GetEntry(p.Replace(Path.DirectorySeparatorChar,'/')).Open();
            }
            p = Path.Combine(ThemePath, p);
            return File.OpenRead(p);
        }

        public void WriteFile<T>(T obj, params string[] path)
        {
            if (zipFile != null) return;
            Utils.SaveObject(obj, Path.Combine(ThemePath, Path.Combine(path)));
        }

        public void Save()
        {
            WriteFile(Config, "theme.json");
            foreach (string k in UIConfig.Keys)
            {
                WriteFile(UIConfig[k], "Interface", k + ".json");
            }
        }
    }
}
