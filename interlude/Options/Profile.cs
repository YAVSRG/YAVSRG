using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;
using Newtonsoft.Json;
using YAVSRG.Gameplay;
using YAVSRG.Gameplay.Watchers;
using static YAVSRG.Charts.DifficultyRating.KeyLayout;

namespace YAVSRG.Options
{
    public class Profile
    {
        [JsonIgnore]
        public string ProfilePath = "Default.json";
        [JsonIgnore]
        public double Rate = 1.0f;

        public string Name = "Default Profile";
        public string UUID = Guid.NewGuid().ToString();
        public int Keymode = 0;
        public string Skin = "_fallback";
        public float ScrollSpeed = 2.05f;
        public bool UseArrowsFor4k = false;
        public int HitPosition = 0;
        public float OD = 8.5f;
        public int Judge = 4;
        public bool Upscroll = false;
        public float ScreenCoverUp = 0f;
        public float ScreenCoverDown = 0f;
        public float PerspectiveTilt = 0f;
        public float BackgroundDim = 0.5f;
        public string ChartSortMode = "Title";
        public string ChartGroupMode = "Pack";
        public IScoreSystem.ScoreType ScoreSystem = IScoreSystem.ScoreType.Default;
        public ColorScheme ColorStyle = new ColorScheme(Colorizer.ColorStyle.Column);
        public float[] AccGradeThresholds = new float[] { 98.5f, 95, 93, 91, 89 };
        public ProfileStats Stats = new ProfileStats();

        //these are default binds
        public Key[][] Bindings = new Key[][] //this needs redoing cause it turns out shit in the profile.json
        {
            null, null, null, //0k 1k 2k (to make indexing easier)
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
    }
}
