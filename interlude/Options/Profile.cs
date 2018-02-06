using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;
using Newtonsoft.Json;
using YAVSRG.Gameplay;

namespace YAVSRG.Options
{
    class Profile
    {
        [JsonIgnore]
        public string ProfilePath = "Default.json";
        public string Name = "Default Profile";
        public int Keymode = 0;
        public string Skin = "_fallback";
        public bool FixedScroll = false;
        public float ScrollSpeed = 2.05f;
        public bool UseArrowsFor4k = false;
        public int HitPosition = 0;
        public double Rate = 1.0f;
        public float OD = 8.5f;
        public int Judge = 4;
        public float ScreenCoverUp = 0f;
        public float ScreenCoverDown = 0f;
        public ScoreType ScoreSystem = ScoreType.Default;
        public ColorScheme ColorStyle = new ColorScheme(Colorizer.ColorStyle.Column);

        //these are default binds
        public Key[][] Bindings = new Key[][] //this needs redoing cause it turns out shit in the profile.json
        {
            null, null, null, //0k 1k 2k
            new Key[] { Key.Left, Key.Down, Key.Right }, //3K
            new Key[] { Key.Z, Key.X, Key.Period, Key.Slash }, //4k
            new Key[] { Key.Z, Key.X, Key.Space, Key.Period, Key.Slash }, //5k
            new Key[] { Key.Z, Key.X, Key.C, Key.Comma, Key.Period, Key.Slash }, //6k
            new Key[] { Key.Z, Key.X, Key.C, Key.Space, Key.Comma, Key.Period, Key.Slash }, //7k
            new Key[] { Key.Z, Key.X, Key.C, Key.V, Key.Comma, Key.Period, Key.Slash, Key.RShift }, //8k
            new Key[] { Key.Z, Key.X, Key.C, Key.V, Key.Space, Key.Comma, Key.Period, Key.Slash, Key.RShift }, //9k
            new Key[] { Key.CapsLock, Key.Q, Key.W, Key.E, Key.V, Key.Space, Key.K, Key.L, Key.Semicolon, Key.Quote }, //10k
        };
    }
}
