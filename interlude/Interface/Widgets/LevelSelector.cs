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
                Move(new Rect(width - (float)Math.Pow((y-ScreenUtils.ScreenHeight+Game.Screens.Toolbar.Height+120) / 48f, 2)*1.5f, y, -50, y + height));
            }

            public float BottomEdge()
            {
                return BottomRight.AbsoluteTargetY;
            }

            public void AddItem(Group i)
            {
                Children.Add(i);
            }

            public void PopOutChildren()
            {
                float y = BottomRight.AbsoluteTargetY;
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
                Game.Screens.DrawChartBackground(bounds, Color.FromArgb(80,fill), 1.5f);
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
                if (ScreenUtils.MouseOver(bounds))
                {
                    TopLeft.Target(TopLeft.AbsoluteTargetX + 150, TopLeft.AbsoluteTargetY);
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

        protected List<Group> groups;
        
        public int scroll = 0;

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            Refresh();
        }

        public void Refresh()
        {
            groups = new List<Group>();
            foreach (ChartLoader.ChartGroup p in ChartLoader.GroupedCharts)
            {
                AddPack(p);
            }
        }

        public void ScrollToSelected()
        {
            int y = scroll;
            foreach (Group g in groups)
            {
                g.UpdatePosition(y);
                y += g.GetHeight();
            }
            foreach (Group g in groups)
            {
                if (g.Highlight())
                {
                    if (!g.Expand)
                        ExpandGroup(g);

                    foreach (Group c in g.Children)
                    {
                        if (c.Highlight())
                        {
                            c.ScrollTo(ref scroll);
                            return;
                        }
                    }
                }
            }
            scroll = 0;
        }

        private void ExpandGroup(Group x)
        {
            bool temp = x.Expand; //remember if the group in question was expanded or not
            foreach (Group c in groups)
            {
                if (c.Expand)
                {
                    if (c.BottomEdge() < x.BottomEdge()) //collapse all groups (includes the one just clicked on)
                    {
                        scroll += c.GetHeight();
                        c.Expand = false; //has to be in this order
                        scroll -= c.GetHeight(); //for this correction to work
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

        public void AddPack(ChartLoader.ChartGroup group)
        {
            Group g = new Group(100, (x) =>
            {
                ExpandGroup(x);
                x.ScrollTo(ref scroll);
            },
            (x) => {
                if (Input.KeyPress(OpenTK.Input.Key.Delete))
                {
                    Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Are you SURE you want to delete ALL CHARTS in this group?",(d) => {
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
                        x.ScrollTo(ref scroll);
                    }
                },
                (x) => {
                    if (Input.KeyPress(OpenTK.Input.Key.Delete))
                    {
                        Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Are you SURE you want to delete this chart from your computer?", (d) => {
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
                        g.ScrollTo(ref scroll);
                    }
                }, () => { return Game.Gameplay.CurrentCachedChart == chart; }, chart.artist + " - " + chart.title, chart.diffname + " ("+chart.keymode.ToString()+"k)", Game.Options.Theme.SelectChart));
            }
            groups.Add(g);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            foreach (Group g in groups)
            {
                g.Draw(bounds);
            }
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            int y = scroll;
            foreach (Group g in groups)
            {
                g.UpdatePosition(y);
                g.Update(bounds);
                y += g.GetHeight();
            }
            if (y < bounds.Height) scroll += 10; //prevents users from scrolling off the list
            if (scroll > 0) scroll -= 10;
            if (Input.KeyPress(OpenTK.Input.Key.Up))
            {
                scroll += 15;
            }
            else if (Input.KeyPress(OpenTK.Input.Key.Down))
            {
                scroll -= 15;
            }
            scroll += Input.MouseScroll * 100;
        }
    }
}
