using System;
using System.Collections.Generic;
using System.Linq;
using System.Drawing;
using Interlude.Gameplay;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class LevelSelector : Widget
    {
        protected class SelectableItem
        {
            List<SelectableItem> Children = new List<SelectableItem>();
            SelectableItem Parent;
            bool Expanded;
            AnimationColorMixer FrameColor, FillColor;
            AnimationSlider MouseOver;
            Color BaseColor;

            public bool ScrollFlag;
            public Func<bool> Highlight = () => false;
            public Func<Color> Colorizer;
            public float Height = 80f;
            public string Title = "?", Subtitle = "";
            public Action OnClick, OnRightClick;

            public SelectableItem(Func<Color> col)
            {
                Colorizer = col;
                OnClick = () => { if (Expanded) Collapse(); else Expand(); };
                OnRightClick = () => { Parent.Collapse(); Parent.ScrollFlag = true; };
                FrameColor = new AnimationColorMixer(Color.White);
                FillColor = new AnimationColorMixer(Game.Screens.BaseColor);
                MouseOver = new AnimationSlider(0);
            }

            public float Draw(ref float topEdge, float minBound, float maxBound)
            {
                if (Parent != null)
                {
                    if (topEdge < maxBound && topEdge + Height > minBound)
                    {
                        if (BaseColor.A == 0)
                        {
                            QuickColor(); //prevents black flashing
                        }
                        Rect bounds = new Rect(ScreenUtils.ScreenWidth - 600 - MouseOver + (float)Math.Pow(topEdge * 0.021, 2) * 1.5f, topEdge, ScreenUtils.ScreenWidth, topEdge + Height);
                        SpriteBatch.DrawTilingTexture("levelselectbase", bounds, 400, 0, 0, FillColor);
                        Game.Screens.DrawChartBackground(bounds, Color.FromArgb(80, FillColor), 1.5f);
                        ScreenUtils.DrawFrame(bounds, FrameColor, components: 187);
                        if (Subtitle == "")
                        {
                            SpriteBatch.Font1.DrawTextToFill(Title, new Rect(bounds.Left + 20, bounds.Top + 22.5f, bounds.Left + 600, bounds.Bottom - 20), FrameColor, true, Utils.ColorInterp(FillColor, Color.Black, 0.7f));
                        }
                        else
                        {
                            SpriteBatch.Font1.DrawTextToFill(Title, new Rect(bounds.Left + 20, bounds.Top + 8f, bounds.Left + 600, bounds.Bottom - 35), FrameColor, true, Utils.ColorInterp(FillColor, Color.Black, 0.5f));
                            SpriteBatch.Font2.DrawTextToFill(Subtitle, new Rect(bounds.Left + 20, bounds.Bottom - 40, bounds.Left + 600, bounds.Bottom - 5), FrameColor, true, Utils.ColorInterp(FrameColor, Color.Black, 0.7f));
                        }
                    }
                    topEdge += Height;
                }
                if (Expanded)
                {
                    foreach (SelectableItem c in Children)
                    {
                        c.Draw(ref topEdge, minBound, maxBound);
                    }
                }
                return topEdge;
            }

            public float Update(ref float topEdge, float minBound, float maxBound)
            {
                if (Parent != null)
                {
                    if (ScrollFlag)
                    {
                        ScrollFlag = false;
                        ScrollTo(topEdge);
                    }
                    if (topEdge < maxBound && topEdge + Height > minBound)
                    {
                        Rect bounds = new Rect(ScreenUtils.ScreenWidth - 600 - MouseOver + (float)Math.Pow(topEdge * 0.021, 2) * 1.5f, topEdge, ScreenUtils.ScreenWidth, topEdge + Height);
                        if (ScreenUtils.MouseOver(bounds))
                        {
                            FillColor.Target(Utils.ColorInterp(BaseColor, Color.White, 0.5f));
                            MouseOver.Target = 150;
                            if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                            {
                                Game.Audio.PlaySFX("click", pitch: 0.8f, volume: 0.5f);
                                OnClick();
                                if (Children.Count == 0)
                                {
                                    ScrollFlag = true;
                                }
                            }
                            else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                            {
                                Game.Audio.PlaySFX("click", pitch: 1.2f, volume: 0.5f);
                                OnRightClick();
                            }
                        }
                        else
                        {
                            FillColor.Target(Highlight() ? BaseColor : Utils.ColorInterp(BaseColor, Color.Black, 0.25f)); //Color by options
                            MouseOver.Target = 0;
                        }
                        if (BaseColor.A == 0 || Height == 100)
                        {
                            QuickColor();
                        }
                        FillColor.Update(); FrameColor.Update(); MouseOver.Update();
                    }
                    else
                    {
                        FillColor.QuickTarget(Utils.ColorInterp(BaseColor, Color.Black, 0.25f));
                        MouseOver.Val = 0;
                    }
                    topEdge += Height;
                }
                if (Expanded)
                {
                    foreach (SelectableItem c in Children)
                    {
                        c.Update(ref topEdge, minBound, maxBound);
                    }
                }
                return topEdge;
            }

            protected void QuickColor()
            {
                BaseColor = Colorizer();
                FillColor.QuickTarget(Utils.ColorInterp(BaseColor, Color.Black, 0.25f));
            }

            protected void ScrollTo(float topEdge)
            {
                GetRoot().Height -= topEdge;
            }

            public void AddChild(SelectableItem item)
            {
                item.Parent = this;
                Children.Add(item);
            }

            public float GetHeight()
            {
                float height = Parent != null ? Height : 0;
                if (Expanded)
                {
                    foreach (SelectableItem c in Children)
                    {
                        height += c.GetHeight();
                    }
                }
                return height;
            }

            public SelectableItem GetRoot()
            {
                if (Parent != null)
                {
                    return Parent.GetRoot();
                }
                return this;
            }

            public void Collapse()
            {
                //GetRoot().Height -= GetHeight() - Height;
                if (Parent != null)
                    Expanded = false;
            }

            public void Expand()
            {
                Expanded = true;
                //GetRoot().Height += GetHeight() - Height;
            }

            public void ExpandToRoot()
            {
                Expand();
                Parent?.ExpandToRoot();
            }

            public IEnumerable<SelectableItem> Iter()
            {
                foreach (SelectableItem c in Children)
                {
                    foreach (SelectableItem i in c.Iter())
                    {
                        yield return i;
                    }
                }
                yield return this;
            }
        }

        protected SelectableItem items;

        public AnimationSlider scroll = new AnimationSlider(0);

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            Refresh();
        }

        //todo: reduce code duplication here
        public void Forward()
        {
            int i;
            for (int g = 0; g < ChartLoader.GroupedCharts.Count; g++)
            {
                if ((i = ChartLoader.GroupedCharts[g].charts.IndexOf(Game.Gameplay.CurrentCachedChart)) >= 0)
                {
                    if (i + 1 == ChartLoader.GroupedCharts[g].charts.Count)
                    {
                        ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[(g + 1) % ChartLoader.GroupedCharts.Count].charts[0], true);
                    }
                    else
                    {
                        ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[g].charts[i + 1], true);
                    }
                    ScrollToSelected();
                    return;
                }
            }
        }

        public void Back()
        {
            int i;
            for (int g = 0; g < ChartLoader.GroupedCharts.Count; g++)
            {
                if ((i = ChartLoader.GroupedCharts[g].charts.IndexOf(Game.Gameplay.CurrentCachedChart)) >= 0)
                {
                    if (i == 0)
                    {
                        var c = ChartLoader.GroupedCharts[(g - 1 + ChartLoader.GroupedCharts.Count) % ChartLoader.GroupedCharts.Count];
                        ChartLoader.SwitchToChart(c.charts[c.charts.Count - 1], true);
                    }
                    else
                    {
                        ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[g].charts[i - 1], true);
                    }
                    ScrollToSelected();
                    return;
                }
            }
        }
        public void ForwardGroup()
        {
            for (int g = 0; g < ChartLoader.GroupedCharts.Count; g++)
            {
                if (ChartLoader.GroupedCharts[g].charts.IndexOf(Game.Gameplay.CurrentCachedChart) >= 0)
                {
                    ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[(g + 1) % ChartLoader.GroupedCharts.Count].charts[0], true);
                    ScrollToSelected();
                    return;
                }
            }
        }

        public void BackGroup()
        {
            for (int g = 0; g < ChartLoader.GroupedCharts.Count; g++)
            {
                if (ChartLoader.GroupedCharts[g].charts.IndexOf(Game.Gameplay.CurrentCachedChart) >= 0)
                {
                    ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[(g - 1 + ChartLoader.GroupedCharts.Count) % ChartLoader.GroupedCharts.Count].charts[0], true);
                    ScrollToSelected();
                    return;
                }
            }
        }

        public void Random()
        {
            int g = new Random().Next(0, ChartLoader.GroupedCharts.Count);
            int i = new Random().Next(0, ChartLoader.GroupedCharts[g].charts.Count);
            ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[g].charts[i], true);
            ScrollToSelected();
        }

        public void Refresh()
        {
            if (!ChartLoader.ColorBy.ContainsKey(Game.Options.Profile.ChartColorMode))
            {
                Game.Options.Profile.ChartColorMode = ChartLoader.ColorBy.Keys.First();
            }
            items = new SelectableItem(() => Color.White);
            foreach (ChartLoader.ChartGroup p in ChartLoader.GroupedCharts)
            {
                SelectableItem pack = new SelectableItem(() => Game.Screens.BaseColor) { Height = 100, Title = p.label, Highlight = () => { return p.charts.Contains(Game.Gameplay.CurrentCachedChart); } };
                foreach (CachedChart chart in p.charts)
                {
                    pack.AddChild(new SelectableItem(() => ChartLoader.ColorBy[Game.Options.Profile.ChartColorMode](chart)) { Highlight = () => { return Game.Gameplay.CurrentCachedChart == chart; },
                        OnClick = () =>
                        {
                            if (Game.Gameplay.CurrentCachedChart == chart)
                            {
                                Game.Gameplay.PlaySelectedChart();
                            }
                            else
                            {
                                ChartLoader.SwitchToChart(chart, true);
                                Input.ChangeIM(null);
                            }
                        },
                        Title = chart.artist + " - " + chart.title, Subtitle = chart.diffname + " (" + chart.keymode.ToString() + "k)" });
                }
                items.AddChild(pack);
            }
            items.Expand();
            ScrollToSelectedAsync();
        }

        public void ScrollToSelected()
        {
            scroll.Val = scroll.Target;
            foreach (SelectableItem g in items.Iter())
            {
                if (g.Height == 80 && g.Highlight())
                {
                    g.ExpandToRoot();
                    g.ScrollFlag = true;
                    break;
                }
            }
        }

        public void ScrollToSelectedAsync()
        {
            Game.Tasks.AddTask((Output) =>
            {
                string id = Game.Gameplay.CurrentCachedChart.GetFileIdentifier();
                foreach (ChartLoader.ChartGroup g in ChartLoader.GroupedCharts)
                {
                    foreach (CachedChart c in g.charts)
                    {
                        if (c.GetFileIdentifier() == id)
                        {
                            Game.Gameplay.CurrentCachedChart = c;
                            return true;
                        }
                    }
                }
                return false;
            }, (b) => { ScrollToSelected(); }, "ScrollToSelected", false);
        }
      
        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float r = scroll + bounds.Top;
            items.Draw(ref r, bounds.Top, bounds.Bottom);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            float r = scroll + bounds.Top;
            items.Height = 0;
            items.Update(ref r, bounds.Top, bounds.Bottom);
            scroll.Target += items.Height;
            scroll.Target = Math.Max(bounds.Height - items.GetHeight(), scroll.Target);
            scroll.Target = Math.Min(scroll.Target, 0);
            if (Game.Options.General.Hotkeys.Up.Held())
            {
                scroll.Target += 15;
            }
            else if (Game.Options.General.Hotkeys.Down.Held())
            {
                scroll.Target -= 15;
            }
            else if (Game.Options.General.Hotkeys.End.Tapped())
            {
                ForwardGroup();
            }
            else if (Game.Options.General.Hotkeys.Start.Tapped())
            {
                BackGroup();
            }
            else if (Game.Options.General.Hotkeys.Next.Tapped())
            {
                Forward();
            }
            else if (Game.Options.General.Hotkeys.Previous.Tapped())
            {
                Back();
            }
            else if (Game.Options.General.Hotkeys.RandomChart.Tapped())
            {
                Random();
            }
            else if (ScreenUtils.MouseOver(bounds) && Input.MousePress(OpenTK.Input.MouseButton.Right))
            {
                scroll.Target = ((Input.MouseY - bounds.Top) / bounds.Height) * (bounds.Height - items.GetHeight());
            }
            scroll.Target += Input.MouseScroll * 100;
            scroll.Update();
        }
    }
}
