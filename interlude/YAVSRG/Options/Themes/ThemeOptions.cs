using System.Drawing;

namespace Interlude.Options
{
    public class ThemeOptions
    {
        public Color[] JudgementColors = new[] { Color.FromArgb(127, 127, 255), Color.FromArgb(0, 255, 255), Color.FromArgb(255, 255, 0), Color.FromArgb(0, 255, 100), Color.FromArgb(0, 0, 255), Color.Fuchsia, Color.FromArgb(255, 0, 0), Color.FromArgb(255, 255, 0), Color.Fuchsia };
        public string[] JudgementNames = new[] { "Ridiculous", "Marvellous", "Perfect", "Good", "Bad", "Boo", "Miss", "OK", "Fumble" };
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
