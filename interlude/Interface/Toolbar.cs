using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface
{
    public class Toolbar : Widget
    {
        Sprite texture, cursor, frame;
        AnimationSlider slide;
        Widget mc;
        bool hidden;

        public Toolbar()
        {
            texture = Content.LoadTextureFromAssets("toolbar");
            frame = Content.LoadTextureFromAssets("frame");
            cursor = Content.LoadTextureFromAssets("cursor");
            AddChild(mc = new MusicControls()
                .PositionTopLeft(0,80,AnchorType.MAX,AnchorType.MIN)
                .PositionBottomRight(-1000,180,AnchorType.MAX,AnchorType.MIN)
                );
            AddChild(
                new Button("buttonback", "", () => { Game.Screens.PopScreen(); })
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(240, 80, AnchorType.MIN, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonstop", "", () => { int m = mc.A.TargetX > 0 ? -1000 : 1000; mc.A.Move(m, 0); mc.B.Move(m, 0); })
                .PositionTopLeft(80, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN)
                );
            slide = new AnimationSlider(0);
            Animation.Add(slide);
        }

        public void Collapse()
        {
            slide.Target = 0;
        }

        public void Expand()
        {
            slide.Target = 80;
        }

        public void SetHidden(bool v)
        {
            hidden = v;
            if (hidden) { Collapse(); }
            else { Expand(); }
        }

        public new float Height
        {
            get { return slide.Val; }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            if (slide > 1)
            {
                float s = (ScreenHeight * 2 - slide * 2) / 24f;
                for (int i = 0; i < 24; i++) //draws the waveform
                {
                    float level = Game.Audio.WaveForm[i * 4] + Game.Audio.WaveForm[i * 4 + 1] + Game.Audio.WaveForm[i * 4 + 2] + Game.Audio.WaveForm[i * 4 + 3];
                    level += 0.01f;
                    level *= slide * 5;
                    SpriteBatch.DrawRect(-ScreenWidth, -ScreenHeight + slide + i * s, -ScreenWidth + level, -ScreenHeight + slide - 2 + (i + 1) * s, Color.FromArgb(100, Game.Screens.HighlightColor));
                    SpriteBatch.DrawRect(ScreenWidth - level, -ScreenHeight + slide + i * s, ScreenWidth, -ScreenHeight + slide - 2 + (i + 1) * s, Color.FromArgb(100, Game.Screens.HighlightColor));
                }

                //SpriteBatch.Draw(texture,-Width, -Height, Width, -Height + 80, Game.Options.Theme.Dark);
                Game.Screens.DrawStaticChartBackground(-ScreenWidth, -ScreenHeight, ScreenWidth, -ScreenHeight + slide, Game.Screens.DarkColor);
                SpriteBatch.DrawFrame(frame, -ScreenWidth - 30, -ScreenHeight - 30, ScreenWidth + 30, -ScreenHeight + slide + 5, 30f, Game.Screens.BaseColor);
                //SpriteBatch.DrawRect(-Width, -Height + 80, Width, -Height + 85, Game.Screens.BaseColor);
                //SpriteBatch.DrawRect(Width-725, -Height, Width-720, -Height + 80, Game.Screens.BaseColor);

                //SpriteBatch.Draw(texture, -Width, Height-80, Width, Height, Game.Options.Theme.Dark);
                Game.Screens.DrawStaticChartBackground(-ScreenWidth, ScreenHeight - slide, ScreenWidth, ScreenHeight, Game.Screens.DarkColor);
                //SpriteBatch.DrawRect(-Width, Height - slide - 5, Width, Height - slide, Game.Screens.BaseColor);
                SpriteBatch.DrawFrame(frame, -ScreenWidth - 30, ScreenHeight - slide - 5, ScreenWidth + 30, ScreenHeight + 30, 30f, Game.Screens.BaseColor);

                base.Draw(left, top + slide - 80, right, bottom);

                SpriteBatch.DrawText(Game.Options.Profile.Name, 30f, -ScreenWidth, ScreenHeight - slide + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.DrawCentredText("Plays: " + Game.Options.Profile.Stats.TimesPlayed.ToString(), 18f, 0, ScreenHeight - slide + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.DrawCentredText("Playtime: " + Utils.FormatTime(Game.Options.Profile.Stats.SecondsPlayed * 1000), 18f, 0, ScreenHeight - slide + 28, Game.Options.Theme.MenuFont);
                SpriteBatch.DrawCentredText("S Ranks: " + Game.Options.Profile.Stats.SRanks, 18f, 0, ScreenHeight - slide + 51, Game.Options.Theme.MenuFont);
                SpriteBatch.DrawJustifiedText(Game.Version, 25f, ScreenWidth, ScreenHeight - slide + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 25f, ScreenWidth, ScreenHeight - slide + 45, Game.Options.Theme.MenuFont);
            }

            if (!hidden) SpriteBatch.Draw(cursor, Input.MouseX, Input.MouseY, Input.MouseX + 48, Input.MouseY + 48, Game.Screens.HighlightColor);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top + slide - 80, right, bottom);
            if (!hidden && Input.KeyTap(OpenTK.Input.Key.T) && Input.KeyPress(OpenTK.Input.Key.ControlLeft))
            {
                if (slide.Target == 0)
                {
                    Expand();
                }
                else
                {
                    Collapse();
                }
            }
        }
    }
}
