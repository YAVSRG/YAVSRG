using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Options
{
    class Theme
    {
        public Color[] NoteColors = new[] { Color.Red, Color.Blue, Color.Yellow, Color.LightBlue, Color.Green, Color.Purple, Color.Cyan, Color.White };
        public Color[] JudgeColors = new[] { Color.FromArgb(0, 255, 255), Color.FromArgb(255, 255, 0), Color.FromArgb(0, 255, 100), Color.FromArgb(0, 0, 255), Color.Fuchsia, Color.FromArgb(255, 0, 0) };
        public Color HoldBody = Color.White;
        public Color PressedReceptor = Color.LightBlue;
        public Color Receptor = Color.White;
        public int ColumnWidth = 150;
        public bool UseColor = false;
        public bool FlipHoldTail = true;
        public bool JudgementPerColumn = false;
        public bool JudgementShowMarv = false;
        public Color MenuFont = Color.White;
        public Color Base = Color.FromArgb(0, 120, 255);
        public Color Dark = Color.FromArgb(127, 0, 60, 130);
        public Color Highlight = Color.FromArgb(190, 220, 255);
        public string[] Judges = new[] { "Marvellous", "Perfect", "Ok", "Bad", "Terrible", "Miss" };

        public Color GetColor(int index)
        {
            if (UseColor && index < NoteColors.Length)
            {
                return NoteColors[index];
            }
            return Color.White;
        }

        public int GetRotation(int column, int keycount)
        {
            if (Game.Options.Profile.UseArrowsFor4k && keycount == 4)
            {
                switch (column)
                {
                    case 0: { return 3; }
                    case 1: { return 0; }
                    case 2: { return 2; }
                    case 3: { return 1; }
                }
            }
            return 0;
        }

        public void DrawNote(Sprite s, float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(s, left, top, right, bottom, GetColor(index), animation, index, GetRotation(column, keycount));
        }

        public void DrawHead(Sprite s, float left, float top, float right, float bottom, int column, int keycount)
        {
            SpriteBatch.Draw(s, left, bottom, right, top, Color.White, GetRotation(column,keycount));
        }

        public void DrawTail(Sprite s, float left, float top, float right, float bottom, int column, int keycount)
        {
            //something funny for arrow tails
            //handle flip logic
            SpriteBatch.Draw(s, left, top, right, bottom, Color.White, GetRotation(column,keycount));
        }

        public void DrawReceptor(Sprite s, float left, float top, float right, float bottom, int column, int keycount, bool pressed)
        {
            SpriteBatch.Draw(s, left, top, right, bottom, pressed ? PressedReceptor : Receptor, GetRotation(column, keycount));
        }

        public Sprite GetNoteTexture(int keycount)
        {
            if (Game.Options.Profile.UseArrowsFor4k && keycount == 4)
            {
                return Content.LoadTextureFromAssets("arrow");
            }
            return Content.LoadTextureFromAssets("note");
        }

        public Sprite GetHeadTexture(int keycount)
        {
            if (Game.Options.Profile.UseArrowsFor4k && keycount == 4)
            {
                return Content.LoadTextureFromAssets("arrowholdhead");
            }
            return Content.LoadTextureFromAssets("holdhead");
        }

        public Sprite GetBodyTexture(int keycount)
        {
            return Content.LoadTextureFromAssets("holdbody");
        }

        public Sprite GetReceptorTexture(int keycount)
        {
            if (Game.Options.Profile.UseArrowsFor4k && keycount == 4)
            {
                return Content.LoadTextureFromAssets("arrowreceptor");
            }
            return Content.LoadTextureFromAssets("receptor");
        }
    }
}
