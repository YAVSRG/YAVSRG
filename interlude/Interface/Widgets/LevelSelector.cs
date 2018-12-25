using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Charts.YAVSRG;
using YAVSRG.Charts;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
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

            public bool ScrollFlag;
            public Func<bool> Highlight = () => false;
            public float Height = 80f;
            public string Title = "?", Subtitle = "";
            public Action OnClick, OnRightClick;

            public SelectableItem()
            {
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
                        Rect bounds = new Rect(ScreenUtils.ScreenWidth - 600 - MouseOver + (float)Math.Pow(topEdge * 0.021, 2) * 1.5f, topEdge, ScreenUtils.ScreenWidth, topEdge + Height);
                        SpriteBatch.DrawTilingTexture("levelselectbase", bounds, 400, 0, 0, FillColor);
                        Game.Screens.DrawChartBackground(bounds, Color.FromArgb(80, FillColor), 1.5f);
                        ScreenUtils.DrawFrame(bounds, 30f, FrameColor, components: 187);
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
                            FillColor.Target(Game.Screens.BaseColor);
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
                            FillColor.Target(Highlight() ? Game.Screens.BaseColor : Game.Screens.DarkColor); //Color by options
                            MouseOver.Target = 0;
                        }
                        FillColor.Update(); FrameColor.Update(); MouseOver.Update();
                    }
                    else
                    {
                        FillColor.Target(Game.Screens.DarkColor);
                        FillColor.Skip(); FrameColor.Skip(); MouseOver.Val = 0;
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

        protected class Group : Widget
        {
            public new List<Group> Children;
            public bool Expand;
            public Func<bool> Highlight;
            public Action<Group> OnClick, OnRightClick;
            private string title;
            private string subtitle;
            private int height;
            private int width;
            private AnimationColorMixer border;
            private AnimationColorMixer fill;
            private Color baseColor;

            public Group(int height, Action<Group> action, Action<Group> rightClickAction, Func<bool> highlight, string line1, string line2, Color c)
            {
                Children = new List<Group>();
                this.height = height;
                title = line1;
                subtitle = line2;
                OnClick = action;
                OnRightClick = rightClickAction;
                Highlight = highlight;
                baseColor = c;
                Animation.Add(border = new AnimationColorMixer(Color.White));
                Animation.Add(fill = new AnimationColorMixer(c));
                PopOut(0);
            }

            public void UpdatePosition(float y)
            {
                width = 600;
                Move(new Rect(width - (float)Math.Pow((y - ScreenUtils.ScreenHeight + Game.Screens.Toolbar.Height + 120) / 48f, 2) * 1.5f, y, -50, y + height), false);
            }

            public float BottomEdge()
            {
                return BottomAnchor.StaticPos(true);
            }

            public void AddItem(Group i)
            {
                Children.Add(i);
            }

            public void PopOutChildren()
            {
                float y = BottomEdge();
                foreach (Group g in Children)
                {
                    g.PopOut(y);
                    y += g.GetHeight();
                }
            }

            public void PopOut(float bottomedge)
            {
                PositionTopLeft(100, bottomedge, AnchorType.MAX, AnchorType.MIN);
                PositionBottomRight(-50, bottomedge + height, AnchorType.MAX, AnchorType.MIN);
                PopOutChildren();
            }

            public virtual int GetHeight()
            {
                int r = height;
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        r += g.GetHeight();
                    }
                }
                return r;
            }

            public void ScrollTo(ref int scroll)
            {
                scroll -= (int)(BottomEdge() - ScreenUtils.ScreenHeight + Game.Screens.Toolbar.Height + height / 2); //move scroll appropriately
            }

            public override void Draw(Rect bounds)
            {
                base.Draw(bounds);
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        g.Draw(bounds);
                    }
                }
                bounds = GetBounds(bounds);
                if (bounds.Top > ScreenUtils.ScreenHeight || bounds.Bottom < -ScreenUtils.ScreenHeight) { return; }
                SpriteBatch.DrawTilingTexture("levelselectbase", bounds, 400, 0, 0, fill);
                Game.Screens.DrawChartBackground(bounds, Color.FromArgb(80, fill), 1.5f);
                ScreenUtils.DrawFrame(bounds, 30f, border);
                if (subtitle == "")
                {
                    SpriteBatch.Font1.DrawTextToFill(title, new Rect(bounds.Left + 20, bounds.Top + 22.5f, bounds.Left + width, bounds.Bottom - 20), border, true, Utils.ColorInterp(fill, Color.Black, 0.7f));
                }
                else
                {
                    SpriteBatch.Font1.DrawTextToFill(title, new Rect(bounds.Left + 20, bounds.Top + 8f, bounds.Left + width, bounds.Bottom - 35), border, true, Utils.ColorInterp(fill, Color.Black, 0.5f));
                    SpriteBatch.Font2.DrawTextToFill(subtitle, new Rect(bounds.Left + 20, bounds.Bottom - 40, bounds.Left + width, bounds.Bottom - 5), border, true, Utils.ColorInterp(border, Color.Black, 0.7f));
                }
            }

            public override void Update(Rect bounds)
            {
                float x = BottomEdge();
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        g.UpdatePosition(x);
                        g.Update(bounds);
                        x += g.GetHeight();
                    }
                }
                Rect parentBounds = bounds;
                bounds = GetBounds(bounds);
                //if (bounds.Top > ScreenUtils.ScreenHeight || bounds.Bottom < -ScreenUtils.ScreenHeight) { BottomRight.Update(); TopLeft.Update(); return; }
                if (ScreenUtils.MouseOver(bounds))
                {
                    LeftAnchor.Move(LeftAnchor.StaticPos(true)+150, false);
                    fill.Target(Utils.ColorInterp(baseColor, Color.White, 0.2f));
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        Game.Audio.PlaySFX("click", pitch: 0.8f, volume: 0.5f);
                        OnClick(this);
                    }
                    else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                    {
                        Game.Audio.PlaySFX("click", pitch: 1.2f, volume: 0.5f);
                        OnRightClick(this);
                    }
                }
                else
                {
                    fill.Target(Highlight() ? Utils.ColorInterp(baseColor, Color.White, 0.5f) : baseColor);
                }
                base.Update(parentBounds);
            }
        }

        //protected List<Group> groups;
        protected SelectableItem items;

        public AnimationSlider scroll = new AnimationSlider(0);

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            Refresh();
        }

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

        public void Random()
        {
            int g = new Random().Next(0, ChartLoader.GroupedCharts.Count);
            int i = new Random().Next(0, ChartLoader.GroupedCharts[g].charts.Count);
            ChartLoader.SwitchToChart(ChartLoader.GroupedCharts[g].charts[i], true);
            ScrollToSelected();
        }

        public void Refresh()
        {
            items = new SelectableItem();
            foreach (ChartLoader.ChartGroup p in ChartLoader.GroupedCharts)
            {
                SelectableItem pack = new SelectableItem() { Height = 100, Title = p.label, Highlight = () => { return p.charts.Contains(Game.Gameplay.CurrentCachedChart); } };
                foreach (CachedChart chart in p.charts)
                {
                    pack.AddChild(new SelectableItem() { Highlight = () => { return Game.Gameplay.CurrentCachedChart == chart; },
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

        /*
        private void ExpandGroup(Group x)
        {
            bool temp = x.Expand; //remember if the group in question was expanded or not
            foreach (Group c in groups)
            {
                if (c.Expand)
                {
                    if (c.BottomEdge() < x.BottomEdge()) //collapse all groups (includes the one just clicked on)
                    {
                        scroll.Target += c.GetHeight();
                        c.Expand = false; //has to be in this order
                        scroll.Target -= c.GetHeight(); //for this correction to work
                    }
                    else
                    {
                        c.Expand = false;
                    }
                }
            }
            x.Expand = !temp; //toggle expansion of this group
            if (x.Expand)
            {
                x.PopOutChildren(); //this makes the expanded items not all come from the same point, they are spread out and offscreen
            }
        }
        /*
        public void AddPack(ChartLoader.ChartGroup group)
        {
            Group g = new Group(100, (x) =>
            {
                ExpandGroup(x);
                //x.ScrollTo(ref scroll.Target);
            },
            (x) =>
            {
                if (Input.KeyPress(OpenTK.Input.Key.Delete))
                {
                    Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Are you SURE you want to delete ALL CHARTS in this group?", (d) =>
                    {
                        if (d == "Y")
                        {
                            ChartLoader.DeleteGroup(group);
                            ChartLoader.Refresh();
                        }
                    }));
                }
            }, () => { return group.charts.Contains(Game.Gameplay.CurrentCachedChart); }, group.label, "", Game.Options.Theme.SelectPack);

            foreach (CachedChart chart in group.charts) //populate group with items
            {
                g.AddItem(new Group(80, (x) =>
                {
                    if (x.Highlight())
                    {
                        Game.Gameplay.PlaySelectedChart();
                    }
                    else
                    {
                        ChartLoader.SwitchToChart(chart, true);
                        Input.ChangeIM(null);
                        //x.ScrollTo(ref scroll);
                    }
                },
                (x) =>
                {
                    if (Input.KeyPress(OpenTK.Input.Key.Delete))
                    {
                        Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Are you SURE you want to delete this chart from your computer?", (d) =>
                        {
                            if (d == "Y")
                            {
                                ChartLoader.DeleteChart(chart);
                                ChartLoader.Refresh();
                            }
                        }));
                    }
                    else if (Input.KeyPress(OpenTK.Input.Key.E))
                    {
                        Game.Screens.AddScreen(new Screens.ScreenEditor());
                    }
                    else
                    {
                        ExpandGroup(g);
                        //g.ScrollTo(ref scroll);
                    }
                }, () => { return Game.Gameplay.CurrentCachedChart == chart; }, chart.artist + " - " + chart.title, chart.diffname + " (" + chart.keymode.ToString() + "k)", Game.Options.Theme.SelectChart));
            }
            groups.Add(g);
        }*/

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
            if (Input.KeyPress(OpenTK.Input.Key.Up))
            {
                scroll.Target += 15;
            }
            else if (Input.KeyPress(OpenTK.Input.Key.Down))
            {
                scroll.Target -= 15;
            }
            else if (Input.KeyTap(OpenTK.Input.Key.Right))
            {
                Forward();
            }
            else if (Input.KeyTap(OpenTK.Input.Key.Left))
            {
                Back();
            }
            else if (Input.KeyTap(OpenTK.Input.Key.F2))
            {
                Random();
            }
            scroll.Target += Input.MouseScroll * 100;
            scroll.Update();
        }
    }
}
