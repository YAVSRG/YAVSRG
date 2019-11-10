using System;
using System.Linq;
using System.Drawing;
using Prelude.Gameplay.Mods;
using Interlude.Interface.Widgets;
using Interlude.Graphics;

namespace Interlude.Interface.Dialogs
{
    //todo: make generic list selection dialog
    class ModSelectDialog : FadeDialog
    {
        FlowContainer Selected, Available;

        class ModSelectCard : TooltipContainer
        {
            ModSelectDialog dialog;
            string mod;

            public ModSelectCard(string ModName, ModSelectDialog d) : base(new Widget(), Mod.AvailableMods[ModName].Description)
            {
                mod = ModName;
                dialog = d;
                Reposition(0, 0, 0, 0, 0, 1, 50, 0);
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (ScreenUtils.CheckButtonClick(bounds))
                {
                    if (Parent == dialog.Selected)
                    {
                        dialog.Selected.RemoveChild(this);
                        dialog.Available.AddChild(this);
                        Game.Gameplay.SelectedMods.Remove(mod);
                        Game.Gameplay.UpdateChart();
                    }
                    else
                    {
                        dialog.Available.RemoveChild(this);
                        dialog.Selected.AddChild(this);
                        Game.Gameplay.SelectedMods.Add(mod, Mod.AvailableMods[mod].DefaultSettings);
                        Game.Screens.AddDialog(new ConfigDialog(
                            (s) =>
                            {
                                Game.Gameplay.UpdateChart();
                            }, "Configure Mod", Game.Gameplay.SelectedMods[mod], Mod.AvailableMods[mod].GetType()
                            ));
                    }
                }
            }

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                SpriteBatch.DrawRect(bounds, Game.Screens.BaseColor);
                SpriteBatch.Font1.DrawCentredTextToFill(Mod.AvailableMods[mod].Name, bounds, Color.White);
            }
        }

        public ModSelectDialog(Action<string> action) : base(action)
        {
            AddChild(new SimpleButton("Done", () => { Close(""); }, () => false, () => Game.Options.General.Hotkeys.Select).Reposition(0, 0.4f, -150, 1, 0, 0.6f, -100, 1));
            AddChild((Selected = new FlowContainer()).Reposition(50, 0.5f, 200, 0, -200, 1, -200, 1));
            AddChild((Available = new FlowContainer()).Reposition(200, 0, 200, 0, -50, 0.5f, -200, 1));
            AddChild(new TextBox("Selected", TextAnchor.RIGHT, 0f, true, Game.Options.Theme.MenuFont, Color.Black).Reposition(0, 0.5f, 50, 0, -200, 1, 200, 0));
            AddChild(new TextBox("Available", TextAnchor.LEFT, 0f, true, Game.Options.Theme.MenuFont, Color.Black).Reposition(200, 0, 50, 0, 0, 0.5f, 200, 0));
            string[] mods = Mod.AvailableMods.Keys.ToArray();
            foreach (string m in mods)
            {
                if (Mod.AvailableMods[m].Visible)
                {
                    if (Game.Gameplay.SelectedMods.ContainsKey(m))
                    {
                        Selected.AddChild(new ModSelectCard(m, this));
                    }
                    else
                    {
                        Available.AddChild(new ModSelectCard(m, this));
                    }
                }
            }
            AddChild(new TextBox(() => Game.Gameplay.GetModString(), TextAnchor.CENTER, 30f, true, () => Game.Options.Theme.MenuFont, () => Color.Black).Reposition(0, 0, -100, 1, 0, 1, 0, 1));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Game.Options.General.Hotkeys.Mods.Tapped())
            {
                OnClosing();
            }
        }
    }
}
