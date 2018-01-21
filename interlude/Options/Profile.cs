using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        public float OD = 8.5f;
        public int Judge = 4;
        public ColorScheme ColorStyle = new ColorScheme( Colorizer.ColorStyle.Column);

        public Key[][] Bindings = new Key[][] //this needs redoing cause it turns out shit in the profile.json
        {
            null, null, null, null, //1K 2K 3K
            //new Key[] { Key.S, Key.D, Key.Keypad4, Key.Keypad5 }, //4k
            new Key[] { Key.Z, Key.X, Key.Period, Key.Slash },
            new Key[] { Key.Z, Key.X, Key.Space, Key.Period, Key.Slash }, //5k
            new Key[] { Key.Z, Key.X, Key.C, Key.Comma, Key.Period, Key.Slash }, //6k
            new Key[] { Key.Z, Key.X, Key.C, Key.Space, Key.Comma, Key.Period, Key.Slash }, //7k
            //new Key[] { Key.A, Key.S, Key.D, Key.Space, Key.Keypad4, Key.Keypad5, Key.Keypad6 }, //7k
        };
    }
}
