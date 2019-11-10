using System.Drawing;
using Newtonsoft.Json;
using Interlude.Interface;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Options
{
    public class ThemeOptions
    {
        public Color[] JudgeColors = new[] { Color.FromArgb(0, 255, 255), Color.FromArgb(255, 255, 0), Color.FromArgb(0, 255, 100), Color.FromArgb(0, 0, 255), Color.Fuchsia, Color.FromArgb(255, 0, 0) };
        public string[] Judges = new[] { "Marvellous", "Perfect", "Good", "Bad", "Yikes", "Miss" };
        public string Font1 = "Akrobat-Black.otf";
        public string Font2 = "Akrobat-Regular.otf";
        public Color MenuFont = Color.White;
        public Color SelectChart = Color.FromArgb(0, 180, 110);
        public Color DefaultThemeColor = Color.FromArgb(0, 160, 255);

        public Color PlayfieldColor = Color.FromArgb(120, 0, 0, 0);
        public int ColumnWidth = 150;
        public float ColumnLightTime = 0.2f;
        public int CursorSize = 50;

        public int NoteColorCount()
        {
            return Game.Options.Themes.GetTexture("note").Rows;
        }
    }
}
