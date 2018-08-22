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
            public List<Group> Children;
            public bool Expand;
            public Func<bool> Highlight;
            public Action<Group> OnClick;
            private string title;
            private string subtitle;
            private int height;
            private int width;
            private AnimationColorMixer border;
            private AnimationColorMixer fill;
            private Color baseColor;

            public Group(int height, Action<Group> action, Func<bool> highlight, string line1, string line2, Color c)
            {
                Children = new List<Group>();
                this.height = height;
                title = line1;
                subtitle = line2;
                OnClick = action;
                Highlight = highlight;
                baseColor = c;
                Animation.Add(border = new AnimationColorMixer(Color.White));
                Animation.Add(fill = new AnimationColorMixer(c));
                PopOut(0);
            }

            public void UpdatePosition(float y)
            {
                width = 600;
                A.Target(width - (float)Math.Pow((y-ScreenUtils.ScreenHeight+Game.Screens.Toolbar.Height+120) / 48f, 2)*1.5f, y);
                B.Target(-50, y + height);
            }

            public float BottomEdge()
            {
                return B.TargetY;
            }

            public void AddItem(Group i)
            {
                Children.Add(i);
            }

            public void PopOutChildren()
            {
                float y = B.TargetY;
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

            public override void Draw(float left, float top, float right, float bottom)
            {
                base.Draw(left, top, right, bottom);
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        g.Draw(left, top, right, bottom);
                    }
                }
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (top > ScreenUtils.ScreenHeight || bottom < -ScreenUtils.ScreenHeight) { return; }
                SpriteBatch.DrawTilingTexture("levelselectbase", left, top, right, bottom, 400, 0, 0, fill);
                Game.Screens.DrawChartBackground(left, top, right, bottom, Color.FromArgb(80,fill), 1.5f);
                SpriteBatch.DrawFrame(left, top, right, bottom, 30, border);
                if (subtitle == "")
                {
                    SpriteBatch.Font1.DrawTextToFill(title, left + 20, top + 22.5f, left + width, bottom - 20, border);
                }
                else
                {
                    SpriteBatch.Font1.DrawTextToFill(title, left + 20, top + 8f, left + width, bottom - 35, border);
                    SpriteBatch.Font2.DrawTextToFill(subtitle, left + 20, bottom - 40, left + width, bottom - 5, border);
                }
            }

            public override void Update(float left, float top, float right, float bottom)
            {
                float x = BottomEdge();
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        g.UpdatePosition(x);
                        g.Update(left, top, right, bottom);
                        x += g.GetHeight();
                    }
                }
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ScreenUtils.MouseOver(left, top, right, bottom))
                {
                    A.MoveTarget(150, 0);
                    fill.Target(Utils.ColorInterp(baseColor, Color.White, 0.2f));
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        OnClick(this);
                    }
                }
                else
                {
                    fill.Target(Highlight() ? Utils.ColorInterp(baseColor, Color.White, 0.5f) : baseColor);
                }
                base.Update(left, top, right, bottom);
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
            foreach (ChartLoader.ChartGroup p in ChartLoader.SearchResult)
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
                }, () => { return Game.Gameplay.CurrentCachedChart == chart; }, chart.artist + " - " + chart.title, chart.diffname + " ("+chart.keymode.ToString()+"k)", Game.Options.Theme.SelectChart));
            }
            groups.Add(g);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            foreach (Group g in groups)
            {
                g.Draw(left, top, right, bottom);
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            int y = scroll;
            foreach (Group g in groups)
            {
                g.UpdatePosition(y);
                g.Update(left, top, right, bottom);
                y += g.GetHeight();
            }
            if (y < bottom-top) scroll += 10; //prevents users from scrolling off the list
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
