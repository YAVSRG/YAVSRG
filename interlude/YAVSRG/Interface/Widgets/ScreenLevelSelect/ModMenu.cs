using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    //todo: rewrite
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

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                float b = color.Val * 5;
                ScreenUtils.DrawFrame(bounds.Expand(b, b), color);
                b = bounds.Height; //variable reuse lol.
                SpriteBatch.Font1.DrawCentredTextToFill(mod, new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + b / 2), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor); //todo: replace with textbox
                string s = Game.Gameplay.SelectedMods.ContainsKey(mod) ? "Enabled" : "Disabled";
                SpriteBatch.Font2.DrawCentredTextToFill(s, new Rect(bounds.Left, bounds.Top + b / 2, bounds.Right, bounds.Bottom), color); //replace with textbox also
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (ScreenUtils.MouseOver(bounds))
                {
                    hover = true;
                    infobox.SetText(Game.Gameplay.Mods[mod].GetDescription(Game.Gameplay.SelectedMods.ContainsKey(mod) ? Game.Gameplay.SelectedMods[mod] : Game.Gameplay.Mods[mod].DefaultSettings));
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        if (Game.Gameplay.SelectedMods.ContainsKey(mod))
                        {
                            Game.Gameplay.SelectedMods.Remove(mod);
                            color.Target = 0;
                        }
                        else
                        {
                            Game.Gameplay.SelectedMods.Add(mod, Game.Gameplay.Mods[mod].DefaultSettings);
                            Game.Screens.AddDialog(new Dialogs.ConfigDialog(
                                (s) => { }, "Configure Mod", Game.Gameplay.SelectedMods[mod], Game.Gameplay.Mods[mod].GetType()
                                ));
                            color.Target = 1;
                        }
                        Game.Gameplay.UpdateChart();
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
            AddChild(info.TL_DeprecateMe(50, 50, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(50, 200, AnchorType.MAX, AnchorType.MIN));

            int x = 50;
            string[] mods = Game.Gameplay.Mods.Keys.ToArray();

            foreach (string m in mods)
            {
                var mb = new ModButton(info, m);
                modbuttons.Add(mb);
                AddChild(mb.TL_DeprecateMe(x, 250, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(x + 100, 350, AnchorType.MIN, AnchorType.MIN));
                x += 100;
            }

            Animation.Add(slide = new AnimationSlider(0));
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            int a = (int)(255 * slide);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);
            SpriteBatch.DrawRect(bounds, Color.Transparent);
            float h = bounds.Height;
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);
            Game.Screens.DrawChartBackground(new Rect(bounds.Left, bounds.Bottom - h * slide, bounds.Right, bounds.Bottom - 1), Color.FromArgb(a, Game.Screens.DarkColor), 1.25f);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.Gameplay.GetModString(), new Rect(bounds.Left + 50, bounds.Bottom - h * slide + 50, bounds.Right - 50, bounds.Bottom - h * slide + 200), Color.FromArgb(a, Game.Options.Theme.MenuFont));

            DrawWidgets(new Rect(bounds.Left, bounds.Bottom - h * slide, bounds.Right, bounds.Bottom));
            ScreenUtils.DrawFrame(bounds.SliceBottom(h * slide), Color.FromArgb(a, Game.Screens.BaseColor), 68);
            SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Bottom - h * slide - 5, bounds.Right, bounds.Bottom - h * slide), Color.FromArgb(a, Game.Screens.BaseColor));
            SpriteBatch.Font2.DrawCentredTextToFill("Mod Select", new Rect(bounds.Left, bounds.Bottom - h * slide - 50, bounds.Right, bounds.Bottom - h * slide), Color.FromArgb(a, Game.Options.Theme.MenuFont));
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);
        }

        public override void Update(Rect bounds)
        {
            if (slide.Val > 0)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (slide.Target > 0)
                {
                    float spacing = (bounds.Width - 100) / (modbuttons.Count + 2f);
                    int i = 1;
                    foreach (var mb in modbuttons)
                    {
                        mb.MoveRelative(new Rect(100 + spacing * i, 250, 200 + spacing * i, 350), bounds);
                        i++;
                    }
                }
                else
                {
                    foreach (var mb in modbuttons)
                    {
                        mb.MoveRelative(new Rect(-150, 250, -50, 350), bounds);
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
