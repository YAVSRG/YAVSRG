using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using OpenTK.Input;
using Newtonsoft.Json;
using Prelude.Utilities;
using Prelude.Gameplay.ScoreMetrics;
using static Prelude.Gameplay.DifficultyRating.KeyLayout;
using static Prelude.Gameplay.ScoreMetrics.ScoreSystem;
using Interlude.Gameplay;

namespace Interlude.Options
{
    public class Profile
    {
        [JsonIgnore]
        public string ProfilePath = "Default.json";
        [JsonIgnore]
        public double Rate = 1.0f;
        [JsonIgnore]
        public Keymode DefaultKeymode
        {
            get { return KeymodePreference ? PreferredKeymode : ToKeymode(Game.CurrentChart != null ? Game.CurrentChart.Keys : 4); }
        }

        public enum Keymode
        {
            Key3 = 0,
            Key4 = 1,
            Key5 = 2,
            Key6 = 3,
            Key7 = 4,
            Key8 = 5,
            Key9 = 6,
            Key10 = 7
        }

        public static Keymode ToKeymode(int keys)
        {
            return (Keymode)(keys - 3);
        }

        public string Name = "Default Profile";
        public string UUID = Guid.NewGuid().ToString();
        public Keymode PreferredKeymode = Keymode.Key4;
        public bool KeymodePreference = false;
        public string Skin = "_fallback";
        public float ScrollSpeed = 2.05f;
        public bool UseArrowsFor4k = false;
        public int HitPosition = 0;
        public bool Upscroll = false;
        public float ScreenCoverUp = 0f;
        public float ScreenCoverDown = 0f;
        public float PerspectiveTilt = 0f;
        public float BackgroundDim = 0.5f;
        public string ChartSortMode = "Title";
        public string ChartGroupMode = "Pack";
        public string ChartColorMode = "Nothing";
        public List<ScoreSystemData> ScoreSystems = new List<ScoreSystemData>();
        public int SelectedScoreSystem;
        //todo: choice of life systems
        public ColorScheme ColorStyle = new ColorScheme(Colorizer.ColorStyle.Column);
        //todo: move to theme since number of ranks available is dependent on theme
        public float[] GradeThresholds = new float[] { 99, 98, 97, 96, 95, 94, 93, 92, 91, 90 };
        public ProfileStats Stats = new ProfileStats();

        //these are default binds
        public Key[][] KeymodeBindings = new Key[][] //this needs redoing cause it turns out shit in the profile.json
        {
            new Key[] { Key.Left, Key.Down, Key.Right }, //3K
            new Key[] { Key.Z, Key.X, Key.Period, Key.Slash }, //4k
            new Key[] { Key.Z, Key.X, Key.Space, Key.Period, Key.Slash }, //5k
            new Key[] { Key.Z, Key.X, Key.C, Key.Comma, Key.Period, Key.Slash }, //6k
            new Key[] { Key.Z, Key.X, Key.C, Key.Space, Key.Comma, Key.Period, Key.Slash }, //7k
            new Key[] { Key.Z, Key.X, Key.C, Key.V, Key.Comma, Key.Period, Key.Slash, Key.RShift }, //8k
            new Key[] { Key.Z, Key.X, Key.C, Key.V, Key.Space, Key.Comma, Key.Period, Key.Slash, Key.RShift }, //9k
            new Key[] { Key.CapsLock, Key.Q, Key.W, Key.E, Key.V, Key.Space, Key.K, Key.L, Key.Semicolon, Key.Quote }, //10k
        };

        public Layout[] KeymodeLayouts = new Layout[] //default playstyles
        {
            Layout.Spread,Layout.Spread,Layout.Spread, //placeholders
            Layout.OneHand, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread
        };

        public ScoreSystem GetScoreSystem(int Index)
        {
            if (Index >= ScoreSystems.Count)
            {
                Index = 0;
            }
            if (ScoreSystems.Count == 0)
            {
                ScoreSystems.Add(new ScoreSystemData(ScoreType.Default, new DataGroup()));
            }
            var ss = ScoreSystems[Index];
            return ss.Instantiate();
        }

        public void Rename(string name)
        {
            Name = name;
            if (ProfilePath == "Default.json") ProfilePath = new Regex("[^a-zA-Z0-9_-]").Replace(name, "") + ".json";
        }
    }
}
