using System;
using System.IO;
using System.Collections.Generic;
using Prelude.Gameplay;
using Interlude.Interface.Animations;
using Prelude.Gameplay.Watchers;
using Prelude.Gameplay.Watchers.HP;
using System.IO.Compression;

namespace Interlude.Gameplay
{
    public class ScoreTracker //handles scoring while you play through a chart, keeping track of hits and acc and stuff
    {
        public event Action<int, int, float> OnHit; //COLUMM, AWARDED JUDGE, MS DELTA

        public ChartWithModifiers Chart;
        public IScoreSystem Scoring;
        public ILifeMeter HP;
        public HitData[] Hitdata;
        public List<IGameplayWatcher> Watchers;
        public AnimationColorFade WidgetColor;
        public int MaxCombo; //max possible combo, not best combo achieved

        public ScoreTracker(ChartWithModifiers c)
        {
            WidgetColor = new AnimationColorFade(System.Drawing.Color.FromArgb(0, Game.Options.Theme.MenuFont), Game.Options.Theme.MenuFont);
            Watchers = new List<IGameplayWatcher>();
            Chart = c;
            Scoring = Game.Options.Profile.GetScoreSystem(Game.Options.Profile.ScoreSystem);
            HP = new HPSystem(Scoring);
            Watchers.Add(Scoring);
            Watchers.Add(HP);

            Scoring.OnHit = (k, j, d) => { OnHit(k, j, d); }; //feed current score system into gameplay ui handlers for hits

            int count = c.Notes.Count;
            Hitdata = new HitData[count];
            for (int i = 0; i < count; i++)
            {
                Hitdata[i] = new HitData(c.Notes.Points[i], c.Keys);
                MaxCombo += c.Notes.Points[i].Count;
            }
            Game.Gameplay.ApplyModsToHitData(c, ref Hitdata);
        }

        public void Update(float time)
        {
            foreach (IGameplayWatcher w in Watchers)
            {
                w.Update(time, Hitdata);
            }
            WidgetColor.Update();
        }

        public void RegisterHit(int index, int column, float delta)
        {
            if (Hitdata[index].hit[column] != 1) { return; } //ignore if the note is already hit or doesn't need to be hit. prevents mashing exploits and such.
            Hitdata[index].hit[column] = 2; //mark that note was not only supposed to be hit, but was also hit (marks it as not a miss)
            Hitdata[index].delta[column] = delta;
            foreach (IGameplayWatcher w in Watchers)
            {
                w.HandleHit(column, index, Hitdata);
            }
        }

        public bool ReachedEnd() //is end of chart?
        {
            return Scoring.ReachedEnd(Hitdata.Length);
        }

        public static string HitDataToString(HitData[] data)
        {
            int k = data[0].hit.Length;
            byte[] result = new byte[data.Length * 5 * k];
            for (int i = 0; i < data.Length; i++)
            {
                Array.Copy(data[i].hit, 0, result, k * (i * 5), k);
                Buffer.BlockCopy(data[i].delta, 0, result, k * (i * 5 + 1), k * 4);
            }
            using (var inputStream = new MemoryStream())
            using (var gZipStream = new GZipStream(inputStream, CompressionLevel.Optimal))
            {
                gZipStream.Write(result, 0, result.Length);
                gZipStream.Close();
                return Convert.ToBase64String(inputStream.ToArray());
            }
        }

        public static HitData[] StringToHitData(string s, int k)
        {
            byte[] raw;
            byte[] compressed = Convert.FromBase64String(s);
            using (var outputStream = new MemoryStream())
            using (var inputStream = new MemoryStream(compressed))
            using (var gZipStream = new GZipStream(inputStream, CompressionMode.Decompress))
            {
                gZipStream.CopyTo(outputStream);
                raw = outputStream.ToArray();
            }
            HitData[] result = new HitData[raw.Length / (5 * k)];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = new HitData(new GameplaySnap(0, 0, 0, 0, 0, 0), k);
                Array.Copy(raw, k * (i * 5), result[i].hit, 0, k);
                Buffer.BlockCopy(raw, k * (i * 5 + 1), result[i].delta, 0, k * 4);
            }
            return result;
        }

        public static void Migrate()
        {
            foreach (string hash in Game.Gameplay.ScoreDatabase.data.Keys)
            {
                ChartSaveData d = Game.Gameplay.ScoreDatabase.data[hash];
                foreach (Score s in d.Scores)
                {
                    byte[] data = Convert.FromBase64String(s.hitdata);
                    using (var inputStream = new MemoryStream())
                    using (var gZipStream = new GZipStream(inputStream, CompressionLevel.Optimal))
                    {
                        gZipStream.Write(data, 0, data.Length);
                        gZipStream.Close();
                        s.hitdata = Convert.ToBase64String(inputStream.ToArray());
                    }
                }
            }

        }
    }
}
