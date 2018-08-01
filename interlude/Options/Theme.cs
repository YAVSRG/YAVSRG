using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using Newtonsoft.Json;

namespace YAVSRG.Options
{
    public class Theme
    {
        [JsonIgnore]
        public WidgetPositionData Gameplay;
        public Color[] JudgeColors = new[] { Color.FromArgb(0, 255, 255), Color.FromArgb(255, 255, 0), Color.FromArgb(0, 255, 100), Color.FromArgb(0, 0, 255), Color.Fuchsia, Color.FromArgb(255, 0, 0) };
        public Color HoldBody = Color.White;
        public Color PressedReceptor = Color.LightBlue;
        public Color Receptor = Color.White;
        public int ColumnWidth = 150;
        public bool FlipHoldTail = true;
        public bool UseHoldTailTexture = true;
        public bool JudgementPerColumn = false;
        public bool JudgementShowMarv = false;
        public string Font1 = "Akrobat Black";
        public string Font2 = "Akrobat";
        public Color MenuFont = Color.White;
        public Color SelectPack = Color.FromArgb(100, 30, 180);
        public Color SelectChart = Color.FromArgb(0, 180, 110);
        public Color SelectDiff = Color.FromArgb(0, 140, 200);
        public Color ThemeColor = Color.FromArgb(0, 255, 160);
        public string[] Judges = new[] { "Marvellous", "Perfect", "Ok", "Bad", "Terrible", "Miss" };

        protected int GetRotation(int column, int keycount)
        {
            if (Game.Options.Profile.UseArrowsFor4k && keycount == 4)
            {
                switch (column)
                {
                    case 0: { return 3; }
                    case 1: { return Game.Options.Profile.Upscroll ? 2 : 0; }
                    case 2: { return Game.Options.Profile.Upscroll ? 0 : 2; }
                    case 3: { return 1; }
                }
            }
            return 0;
        }

        public void DrawNote(float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(NoteTexture(keycount), left, bottom, right, top, Color.White, animation, index, GetRotation(column, keycount));
            //SpriteBatch.Draw("noteoverlay", left, bottom, right, top, GetColor(index), animation, index, GetRotation(column, keycount), depth: -20f);
        }

        public void DrawMine(float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(MineTexture(keycount), left, top, right, bottom, Color.White, animation, index, 0); //fix it later
        }

        public void DrawHead(float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(HeadTexture(keycount), left, bottom, right, top, Color.White, animation, index, GetRotation(column, keycount));
        }

        public void DrawTail(float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            int rotation = UseHoldTailTexture ? 0 : GetRotation(column, keycount);
            if (FlipHoldTail && !UseHoldTailTexture)
            {
                SpriteBatch.Draw(TailTexture(keycount), left, top, right, bottom, Color.White, animation, index, rotation);
            }
            else
            {
                SpriteBatch.Draw(TailTexture(keycount), left, bottom, right, top, Color.White, animation, index, rotation);
            }
        }

        public void DrawHold(float left, float top, float right, float bottom, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(BodyTexture(keycount), left, top, right, bottom, Color.White, animation, index, 0);
        }

        public void DrawReceptor(float left, float top, float right, float bottom, int column, int keycount, bool pressed)
        {
            SpriteBatch.Draw(ReceptorTexture(keycount), left, top, right, bottom, pressed ? PressedReceptor : Receptor, GetRotation(column, keycount), depth: pressed ? -20f : 0f);
        }

        protected string Arrow(int keycount)
        {
            return (Game.Options.Profile.UseArrowsFor4k && keycount == 4) ? "arrow" : "";
        }

        protected string NoteTexture(int keycount)
        {
            return (Game.Options.Profile.UseArrowsFor4k && keycount == 4) ? "arrow" : "note";
        }

        protected string HeadTexture(int keycount)
        {
            return Arrow(keycount) + "holdhead";
        }

        protected string TailTexture(int keycount)
        {
            return Arrow(keycount) + "hold" + (UseHoldTailTexture ? "tail" : "head");
        }

        protected string BodyTexture(int keycount)
        {
            return "holdbody";
        }

        protected string ReceptorTexture(int keycount)
        {
            return Arrow(keycount) + "receptor";
        }

        protected string MineTexture(int keycount)
        {
            return "mine";
        }

        public int CountNoteColors(int keycount)
        {
            return Content.GetTexture(NoteTexture(keycount)).UV_Y;
        }

        public void LoadTextures(int keycount)
        {
            Content.GetTexture(MineTexture(keycount));
            Content.GetTexture(NoteTexture(keycount));
            Content.GetTexture(HeadTexture(keycount));
            Content.GetTexture(TailTexture(keycount));
            Content.GetTexture(BodyTexture(keycount));
            Content.GetTexture(ReceptorTexture(keycount));
        }
    }
}
