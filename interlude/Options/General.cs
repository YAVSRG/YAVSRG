using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Options
{
    public class General
    {
        public enum WindowType
        {
            Window,
            Borderless,
            Fullscreen
        }

        public float UniversalAudioOffset = 0f;
        public float AudioVolume = 0.1f;
        public int FrameLimiter = 0;
        public WindowType WindowMode = WindowType.Borderless;
        public string CurrentProfile = "Default.json";
        public string WorkingDirectory = System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "YAVSRG");
        public Keybinds Binds = new Keybinds();
    }
}
