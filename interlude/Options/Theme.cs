using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using Newtonsoft.Json;
using YAVSRG.Interface;

namespace YAVSRG.Options
{
    public class Theme
    {
        [JsonIgnore]
        public WidgetPositionData Gameplay;
        public Color[] JudgeColors = new[] { Color.FromArgb(0, 255, 255), Color.FromArgb(255, 255, 0), Color.FromArgb(0, 255, 100), Color.FromArgb(0, 0, 255), Color.Fuchsia, Color.FromArgb(255, 0, 0) };
        public string[] Judges = new[] { "Marvellous", "Perfect", "Ok", "Bad", "Terrible", "Miss" };
        public string Font1 = "Akrobat Black";
        public string Font2 = "Akrobat";
        public Color MenuFont = Color.White;
        public Color SelectPack = Color.FromArgb(100, 30, 180);
        public Color SelectChart = Color.FromArgb(0, 180, 110);
        public Color SelectDiff = Color.FromArgb(0, 140, 200);
        public Color ThemeColor = Color.FromArgb(0, 255, 160);
        public bool JudgementPerColumn = false;
        public bool JudgementShowMarv = false;
        public int ColumnWidth = 150;
        public float NoteDepth = 20f;
        public bool FlipHoldTail = true;
        public bool UseHoldTailTexture = true;

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

        public void DrawNote(Rect bounds, int column, int keycount, int index, int animation)
        {
            bounds = bounds.FlipY(); //all these flips are to make downscroll the right way up
            SpriteBatch.Draw(NoteTexture(keycount), bounds, Color.White, animation, index, GetRotation(column, keycount));
            SpriteBatch.Draw(NoteTexture(keycount) + "-overlay", bounds, Color.White, animation, index, GetRotation(column, keycount), depth: -NoteDepth);
        }

        public void DrawMine(Rect bounds, int column, int keycount, int index, int animation)
        {
            bounds = bounds.FlipY();
            SpriteBatch.Draw(MineTexture(keycount), bounds, Color.White, animation, index, 0);
            SpriteBatch.Draw(MineTexture(keycount) + "-overlay", bounds, Color.White, animation, index, 0, depth: -NoteDepth);
        }

        public void DrawHead(Rect bounds, int column, int keycount, int index, int animation)
        {
            bounds = bounds.FlipY();
            SpriteBatch.Draw(HeadTexture(keycount), bounds, Color.White, animation, index, GetRotation(column, keycount));
            SpriteBatch.Draw(HeadTexture(keycount) + "-overlay", bounds, Color.White, animation, index, GetRotation(column, keycount), depth: -NoteDepth);
        }

        public void DrawTail(Rect bounds, int column, int keycount, int index, int animation)
        {
            int rotation = UseHoldTailTexture ? 0 : GetRotation(column, keycount);
            if (!(FlipHoldTail && !UseHoldTailTexture))
            {
                bounds = bounds.FlipY();
            }
            SpriteBatch.Draw(TailTexture(keycount), bounds, Color.White, animation, index, rotation);
            SpriteBatch.Draw(TailTexture(keycount) + "-overlay", bounds, Color.White, animation, index, rotation, depth: -NoteDepth);
        }

        public void DrawHold(Rect bounds, int column, int keycount, int index, int animation)
        {
            SpriteBatch.Draw(BodyTexture(keycount), bounds, Color.White, animation, index, 0);
            SpriteBatch.Draw(BodyTexture(keycount) + "-overlay", bounds, Color.White, animation, index, 0, depth: -NoteDepth);
        }

        public void DrawReceptor(Rect bounds, int column, int keycount, bool pressed)
        {
            SpriteBatch.Draw(ReceptorTexture(keycount), bounds, Color.White, 0, pressed ? 1 : 0, GetRotation(column, keycount));
            SpriteBatch.Draw(ReceptorTexture(keycount) + "-overlay", bounds, Color.White, 0, pressed ? 1 : 0, GetRotation(column, keycount), depth: -NoteDepth);
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

        public void LoadTextures(int keycount) //makes sure they're all in memory to avoid lag spikes while playing
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
