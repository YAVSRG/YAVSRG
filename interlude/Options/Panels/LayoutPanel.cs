using System;
using System.Collections.Generic;
using System.Linq;
using OpenTK.Input;
using Prelude.Gameplay.DifficultyRating;
using Interlude.Interface;
using Interlude.Interface.Widgets;
using Interlude.IO;

namespace Interlude.Options.Panels
{
    class LayoutPanel : OptionsPanel
    {
        private Widget selectKeyMode, selectLayout;
        private KeyBinder[] binds = new KeyBinder[10];
        private ColorPicker[] colors = new ColorPicker[10];
        private int keyMode = (int)Game.Options.Profile.DefaultKeymode + 3;
        private float width;
        private InfoBox infobox;

        protected class ColorPicker : Widget
        {
            string label;
            Action<int> select;
            Func<int> get;
            int max;
            bool hover;

            public ColorPicker(string label, Action<int> select, Func<int> get, int max)
            {
                Change(label, select, get, max);
            }

            public void Change(string label, Action<int> select, Func<int> get, int max)
            {
                this.label = label;
                this.select = select;
                this.get = get;
                this.max = max;
            }

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                bounds = GetBounds(bounds);
                Game.Options.Theme.DrawNote(bounds, 1, ((LayoutPanel)Parent).keyMode, get(), 0);
            }

            public override void Update(Rect bounds)
            {
                base.Update(bounds);
                bounds = GetBounds(bounds);
                if (ScreenUtils.MouseOver(bounds))
                {
                    if (Input.MouseClick(MouseButton.Left))
                    {
                        select(Utils.Modulus(get() + 1, max));
                    }
                    else if (Input.MouseClick(MouseButton.Right))
                    {
                        select(Utils.Modulus(get() - 1, max));
                    }
                    ((LayoutPanel)Parent).infobox.SetText(label);
                    hover = true;
                }
                else if (hover)
                {
                    hover = false;
                    ((LayoutPanel)Parent).infobox.SetText("");
                }
            }
        }

        public LayoutPanel(InfoBox ib) : base(ib, "Key Layout")
        {
            infobox = ib;
            width = ScreenUtils.ScreenWidth * 2 - 600;
            selectKeyMode = new TextPicker("Keys", new string[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, (int)Game.Options.Profile.DefaultKeymode, (i) => { ChangeKeyMode(i + 3, width); })
                .PositionTopLeft(-50, 100, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(50, 150, AnchorType.CENTER, AnchorType.MIN);
            for (int i = 0; i < 10; i++)
            {
                binds[i] = new KeyBinder("Column " + (i + 1).ToString(), Key.F35, (b) => { });
                AddChild(binds[i]);
                colors[i] = new ColorPicker("", null, null, 1);
                AddChild(colors[i]);
            }
            AddChild(selectKeyMode);
            AddChild(new BoolPicker("Different colors per keymode", !Game.Options.Profile.ColorStyle.UseForAllKeyModes, (i) => { Game.Options.Profile.ColorStyle.UseForAllKeyModes = !i; Refresh(); })
                .PositionTopLeft(-500, 525, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(-200, 575, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new TextPicker("Skin", Options.Skins, Math.Max(0, Array.IndexOf(Options.Skins, Game.Options.Profile.Skin)), (i) => { Game.Options.Profile.Skin = Options.Skins[i]; Content.ClearStore(); ChangeKeyMode(keyMode, width); })
                .PositionTopLeft(200, 525, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(500, 575, AnchorType.CENTER, AnchorType.MIN));
        }

        public void Refresh()
        {
            if (Game.Options.Profile.KeymodePreference)
            {
                keyMode = (int)Game.Options.Profile.PreferredKeymode + 3;
                selectKeyMode.SetState(WidgetState.DISABLED);
            }
            else
            {
                selectKeyMode.SetState(WidgetState.NORMAL);
            }
            ChangeKeyMode(keyMode, width);
        }

        private Action<Key> BindSetter(int i, int k)
        {
            return (key) => { Game.Options.Profile.KeymodeBindings[k - 3][i] = key; };
        }
        private Action<int> ColorSetter(int i, int k)
        {
            return (s) => { Game.Options.Profile.ColorStyle.SetColorIndex(i, k, s); };
        }
        private Func<int> ColorGetter(int i, int k)
        {
            return () => { return Game.Options.Profile.ColorStyle.GetColorIndex(i, k); };
        }

        private void ChangeKeyMode(int k, float Width)
        {
            for (int i = 0; i < 10; i++)
            {
                binds[i].SetState(WidgetState.DISABLED);
                colors[i].SetState(WidgetState.DISABLED);
            }
            keyMode = k;
            int c = k * Game.Options.Theme.ColumnWidth > Width ? (int)(Width / k) : Game.Options.Theme.ColumnWidth;
            int start = -k * c / 2;
            for (int i = 0; i < k; i++)
            {
                binds[i].Change(Game.Options.Profile.KeymodeBindings[k - 3][i], BindSetter(i, k));
                binds[i].SetState(WidgetState.NORMAL);
                binds[i].PositionTopLeft(start + i * c, 200, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(start + c + i * c, 250, AnchorType.CENTER, AnchorType.MIN);
            }

            int colorCount = Game.Options.Profile.ColorStyle.GetColorCount(k);
            int availableColors = Game.Options.Theme.CountNoteColors(k);
            c = colorCount * Game.Options.Theme.ColumnWidth > Width ? (int)(Width / colorCount) : Game.Options.Theme.ColumnWidth;
            start = -colorCount * c / 2;
            int keymodeIndex = Game.Options.Profile.ColorStyle.UseForAllKeyModes ? 0 : k;
            for (int i = 0; i < colorCount; i++)
            {
                colors[i].Change(Game.Options.Profile.ColorStyle.GetDescription(i), ColorSetter(i, keymodeIndex), ColorGetter(i, keymodeIndex), availableColors);
                colors[i].SetState(WidgetState.NORMAL);
                colors[i].PositionTopLeft(start + i * c, 300, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(start + c + i * c, 300 + Game.Options.Theme.ColumnWidth, AnchorType.CENTER, AnchorType.MIN);
            }
            if (selectLayout != null)
            {
                Children.Remove(selectLayout);
            }
            List<KeyLayout.Layout> layouts = KeyLayout.GetPossibleLayouts(k);
            string[] layoutNames = layouts.Select((x) => KeyLayout.GetLayoutName(x, k)).ToArray();
            Children.Add(selectLayout = new TextPicker("Keyboard layout", layoutNames, Math.Max(0, layouts.IndexOf(Game.Options.Profile.KeymodeLayouts[k])), (i) => { Game.Options.Profile.KeymodeLayouts[k] = layouts[i]; })
                .PositionTopLeft(-150, 600, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(150, 650, AnchorType.CENTER, AnchorType.MIN));
        }
    }
}