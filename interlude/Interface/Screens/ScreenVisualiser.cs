using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using OpenTK;
using YAVSRG.Charts;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Screens
{
    class ScreenVisualiser : Screen
    {
        AnimationCounter rotation;
        AnimationSlider hideUI;
        int oldmx, oldmy;

        public ScreenVisualiser()
        {
            AddChild(
                new Button("buttonplay", "", () => { Game.Audio.Play(); })
                .PositionTopLeft(250, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(170, 90, AnchorType.MAX, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonpause", "", () => { Game.Audio.Pause(); })
                .PositionTopLeft(170, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(90, 90, AnchorType.MAX, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonstop", "", () => { Game.Audio.Stop(); })
                .PositionTopLeft(90, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(10, 90, AnchorType.MAX, AnchorType.MIN)
                );
            Animation.Add(rotation = new AnimationCounter(31415926, true));
            Animation.Add(hideUI = new AnimationSlider(1f));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Logo.A.Target(-400, -400);
            Game.Screens.Logo.B.Target(400, 400);
            Game.Screens.BackgroundDim.Target = 1;
            Game.Screens.Toolbar.Collapse();
            Game.Screens.Parallax.Target *= 4;
            Game.Audio.OnPlaybackFinish = () => { NextTrack(); };
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Screens.Logo.A.Target(-ScreenUtils.ScreenWidth - 400, -200);
            Game.Screens.Logo.B.Target(-ScreenUtils.ScreenWidth, 200);
            Game.Screens.BackgroundDim.Target = 0.3f;
            Game.Screens.Toolbar.cursor = true;
            Input.LockMouse = false;
            Game.Screens.Toolbar.Expand();
            Game.Screens.Parallax.Target *= 0.25f;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            int alpha = (int)(hideUI * 255);
            base.Draw(left, top, right, bottom);
            float l = 1f + Game.Audio.Level - (0.05f) * Utils.GetBeat(2);
            float r1, r2;
            double a1, a2;
            float rotate = rotation.value * 0.002f;
            for (int i = 0; i < 32; i++) //draws the waveform
            {
                float level = 0;
                for (int t = 0; t < 8; t++)
                {
                    level += Game.Audio.WaveForm[i * 8 + t];
                }
                level *= 0.15f;
                level += 10f;
                r1 = (300 - level * 0.2f) * l;
                r2 = (300 + level) * l;
                a1 = rotate + Math.PI / 192 * i;
                for (int p = 0; p < 6; p++)
                {
                    a2 = a1 + Math.PI / 192;
                    a1 = rotate + rotate - a1;
                    a2 = rotate + rotate - a2;
                    SpriteBatch.Draw("", color: Color.FromArgb(100, Game.Screens.HighlightColor), coords: new Vector2[] { new Vector2(r1 * (float)Math.Sin(a1), r1 * (float)Math.Cos(a1)), new Vector2(r2 * (float)Math.Sin(a1), r2 * (float)Math.Cos(a1)), new Vector2(r2 * (float)Math.Sin(a2), r2 * (float)Math.Cos(a2)), new Vector2(r1 * (float)Math.Sin(a2), r1 * (float)Math.Cos(a2)) });
                    a1 = rotate + rotate - a1;
                    a2 = rotate + rotate - a2;
                    SpriteBatch.Draw("", color: Color.FromArgb(100, Game.Screens.HighlightColor), coords: new Vector2[] { new Vector2(r1 * (float)Math.Sin(a1), r1 * (float)Math.Cos(a1)), new Vector2(r2 * (float)Math.Sin(a1), r2 * (float)Math.Cos(a1)), new Vector2(r2 * (float)Math.Sin(a2), r2 * (float)Math.Cos(a2)), new Vector2(r1 * (float)Math.Sin(a2), r1 * (float)Math.Cos(a2)) });
                    a1 += Math.PI / 3;
                }
            }

            SpriteBatch.DrawRect(left, bottom - 30, right, bottom, Game.Screens.DarkColor);
            SpriteBatch.DrawRect(left + 5, bottom - 25, left + 5 + (right - 10 - left) * Game.Audio.NowPercentage(), bottom - 5,  Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, left + 500, top, right - 500, -330, Color.FromArgb(alpha, Game.Options.Theme.MenuFont));
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.FormatTime((float)Game.Audio.Now()), left, bottom - 150, left + 200, bottom - 75, Color.FromArgb(alpha, Game.Options.Theme.MenuFont));
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.FormatTime((float)Game.Audio.Duration), right-200, bottom - 150, right, bottom - 75, Color.FromArgb(alpha, Game.Options.Theme.MenuFont));
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            float f = Utils.GetBeat(1);
            float r = 400 + Utils.GetBeat(1) * 20;
            Game.Screens.Logo.A.Target(-r, -r);
            Game.Screens.Logo.B.Target(r, r);

            if (hideUI.Val < 0.99f)
            {
                int nx = (int)(ScreenUtils.ScreenWidth * Math.Sin(rotation.value * 0.002f));
                int ny = (int)(ScreenUtils.ScreenHeight * Math.Sin(rotation.value * 0.004f));
                Input.MouseX = (int)(nx + (oldmx - nx) * hideUI);
                Input.MouseY = (int)(ny + (oldmy - ny) * hideUI);
            }

            if (Input.LockMouse && hideUI.Val >= 0.99f && hideUI.Target == 1)
            {
                Input.LockMouse = false;
                Game.Screens.Toolbar.cursor = true;
            }

            if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
            {
                hideUI.Target = 1 - hideUI.Target;
                if (hideUI.Target == 0)
                {
                Game.Screens.Toolbar.cursor = false;
                    Input.LockMouse = true;
                    oldmx = Input.MouseX;
                    oldmy = Input.MouseY;
                }
            }

            if (Input.KeyTap(OpenTK.Input.Key.Right))
            {
                NextTrack();
            }
        }

        public void NextTrack()
        {
            bool flag = false;
            foreach (ChartLoader.ChartGroup g in ChartLoader.Groups)
            {
                foreach (CachedChart c in g.charts)
                {
                    if (c.title == Game.CurrentChart.Data.Title && c.artist == Game.CurrentChart.Data.Artist && c.diffname == Game.CurrentChart.Data.DiffName)
                    {
                        flag = true;
                    }
                    else if (flag)
                    {
                        ChartLoader.SwitchToChart(c, false);
                        return;
                    }
                }
            }
        }

        public void PrevTrack()
        {
            CachedChart prev = null;
            foreach (ChartLoader.ChartGroup g in ChartLoader.Groups)
            {
                foreach (CachedChart c in g.charts)
                {
                    if (c.title == Game.CurrentChart.Data.Title && c.artist == Game.CurrentChart.Data.Artist && c.diffname == Game.CurrentChart.Data.DiffName)
                    {
                        ChartLoader.SwitchToChart(prev, false);
                        return;
                    }
                    else
                    {
                        prev = c;
                    }
                }
            }
        }
    }
}
