using OpenTK;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    class ModMenu : Widget
    {
        class ModButton : Widget
        {
            InfoBox infobox;
            string mod;
            bool hover;
            AnimationColorFade color;

            public ModButton(InfoBox ib, string mod)
            {
                this.mod = mod;
                infobox = ib;
                Animation.Add(color = new AnimationColorFade(Color.White, Game.Screens.BaseColor));
                if (Game.Gameplay.SelectedMods.ContainsKey(mod))
                {
                    color.Target = 1;
                }
            }

            public override void Draw(float left, float top, float right, float bottom)
            {
                base.Draw(left, top, right, bottom);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                float b = color.Val * 5;
                SpriteBatch.DrawFrame(left-b, top-b, right+b, bottom+b, 20f, color);
                b = (bottom - top);
                SpriteBatch.Font1.DrawCentredTextToFill(mod, left, top, right, top + b/2, Game.Options.Theme.MenuFont);
                string s = "Off";
                if (Game.Gameplay.SelectedMods.ContainsKey(mod))
                {
                    s = Game.Gameplay.SelectedMods[mod] == "" ? "On" : Game.Gameplay.SelectedMods[mod];
                }
                SpriteBatch.Font2.DrawCentredTextToFill(s, left, top + b / 2, right, bottom, color);
            }

            public override void Update(float left, float top, float right, float bottom)
            {
                base.Update(left, top, right, bottom);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ScreenUtils.MouseOver(left, top, right, bottom))
                {
                    hover = true;
                    infobox.SetText(Game.Gameplay.Mods[mod].GetDescription(Game.Gameplay.SelectedMods.ContainsKey(mod) ? Game.Gameplay.SelectedMods[mod] : ""));
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        string[] o = Game.Gameplay.Mods[mod].Settings;
                        if (Game.Gameplay.SelectedMods.ContainsKey(mod))
                        {
                            int i = Array.IndexOf(o, Game.Gameplay.SelectedMods[mod]);
                            if (i + 1 < o.Length)
                            {
                                Game.Gameplay.SelectedMods[mod] = o[i + 1];
                            }
                            else
                            {
                                Game.Gameplay.SelectedMods.Remove(mod);
                                color.Target = 0;
                            }
                        }
                        else
                        {
                            Game.Gameplay.SelectedMods.Add(mod, o.Length == 0 ? "" : Game.Gameplay.Mods[mod].Settings[0]);
                            color.Target = 1;
                        }
                    }
                }
                else if (hover)
                {
                    hover = false;
                    infobox.SetText("");
                }
            }
        }

        InfoBox info;
        AnimationSlider slide;
        List<ModButton> modbuttons;

        public ModMenu()
        {
            info = new InfoBox();
            modbuttons = new List<ModButton>();
            AddChild(info.PositionTopLeft(50, 50, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(50, 200, AnchorType.MAX, AnchorType.MIN));

            int x = 50;
            string[] mods = Game.Gameplay.Mods.Keys.ToArray();

            foreach (string m in mods)
            {
                var mb = new ModButton(info, m);
                modbuttons.Add(mb);
                AddChild(mb.PositionTopLeft(x, 250, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(x + 100, 350, AnchorType.MIN, AnchorType.MIN));
                x += 100;
            }

            Animation.Add(slide = new AnimationSlider(0));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            int a = (int)(255 * slide);
            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left, top, right, bottom, Color.Transparent);
            float h = bottom - top;
            SpriteBatch.StencilMode(2);
            Game.Screens.DrawChartBackground(left, bottom - h * slide, right, bottom - 1, Color.FromArgb(a, Game.Screens.DarkColor), 1.25f);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.Gameplay.GetModString(), left + 50, bottom - h * slide + 50, right - 50, bottom - h * slide + 200, Color.FromArgb(a, Game.Options.Theme.MenuFont));

            DrawWidgets(left, bottom - h * slide, right, bottom);
            SpriteBatch.Draw("frame", right - 30, bottom - h * slide, right, bottom, Color.FromArgb(a, Game.Screens.BaseColor), 2, 1);
            SpriteBatch.DrawRect(left, bottom - h * slide - 5, right, bottom - h * slide, Color.FromArgb(a, Game.Screens.BaseColor));
            SpriteBatch.Font2.DrawCentredTextToFill("Mod Select", left, bottom - h * slide - 50, right, bottom - h * slide, Color.FromArgb(a, Game.Options.Theme.MenuFont));
            SpriteBatch.StencilMode(0);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            if (slide.Val > 0)
            {
                base.Update(left, top, right, bottom);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (slide.Target > 0)
                {
                    float spacing = (right - left - 100) / (modbuttons.Count + 2f);
                    int i = 1;
                    foreach (var mb in modbuttons)
                    {
                        mb.A.Target(100 + spacing * i, 250);
                        mb.B.Target(200 + spacing * i, 350);
                        i++;
                    }
                }
                else
                {
                    foreach (var mb in modbuttons)
                    {
                        mb.A.Target(-150, 250);
                        mb.B.Target(-50, 350);
                    }
                }
            }
            else
            {
                Animation.Update();
            }
        }

        public void Toggle()
        {
            slide.Target = 1 - slide.Target;
        }
    }
}
