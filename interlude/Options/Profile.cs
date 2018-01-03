using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static YAVSRG.Options.Colorizer;
using OpenTK.Input;

namespace YAVSRG.Options
{
    class Profile
    {
        public string Name = "Default Profile";
        public int Keymode = 0;
        public string Skin = "_fallback";
        public bool FixedScroll = false;
        public float ScrollSpeed = 2.05f;
        public float Rate = 1.0f;
        public float HitWindow = 45f;
        public ColorScheme ColorStyle = ColorScheme.Column; //has no effect atm

        public Key[][] Bindings = new Key[][]
        {
            null, null, null, null, //1K 2K 3K
            //new Key[] { Key.S, Key.D, Key.Keypad4, Key.Keypad5 }, //4k
            new Key[] { Key.Z, Key.X, Key.Period, Key.Slash },
            new Key[] { Key.Z, Key.X, Key.Space, Key.Period, Key.Slash }, //5k
            new Key[] { Key.Z, Key.X, Key.C, Key.Comma, Key.Period, Key.Slash }, //6k
            new Key[] { Key.Z, Key.X, Key.C, Key.Space, Key.Comma, Key.Period, Key.Slash }, //7k
            //new Key[] { Key.A, Key.S, Key.D, Key.Space, Key.Keypad4, Key.Keypad5, Key.Keypad6 }, //7k
        };

        public float[] HitWindows()
        {
            return new float[] { Game.Options.Profile.HitWindow / 2, Game.Options.Profile.HitWindow, Game.Options.Profile.HitWindow * 2, Game.Options.Profile.HitWindow * 3 };
        }

        public int JudgeHit(float delta) //add one with fixed j4 or something for standardising scores
        {
            if (delta < HitWindow * 0.5f)
            {
                return 0;
            }
            if (delta < HitWindow)
            {
                return 1;
            }
            if (delta < HitWindow * 2)
            {
                return 2;
            }
            return 3;
        }
    }
}
