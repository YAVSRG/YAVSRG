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
            private string title;
            private string subtitle;
            private int height;
            private int width;
            private Action<Group> OnClick;
            private Func<bool> Highlight;
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
                RecursivePopOut(0);
            }

            public void UpdatePosition(float y)
            {
                width = (int)(600);
                A.Target(width - (float)Math.Pow((y-ScreenUtils.ScreenHeight+Game.Screens.toolbar.Height+120) / 48f, 2)*1.5f, y);
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

            public void RecursivePopOutRooted()
            {
                foreach (Group g in Children)
                {
                    g.RecursivePopOut(B.AbsY);
                }
            }

            public void RecursivePopOut(float bottomedge)
            {
                PositionTopLeft(100, bottomedge, AnchorType.MAX, AnchorType.MIN);
                PositionBottomRight(-50, bottomedge + height, AnchorType.MAX, AnchorType.MIN);
                RecursivePopOutRooted();
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
                SpriteBatch.DrawTilingTexture(box, left, top, right, bottom, 400, 0, 0, fill);
                Game.Screens.DrawChartBackground(left, top, right, bottom, Color.FromArgb(80,fill), 0.5f);
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
                    A.Move(150, 0);
                    fill.Target(Utils.ColorInterp(baseColor, Color.White, 0.2f));
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        OnClick(this);
                    }
                }
                else
                {
                    fill.Target(Highlight() ? Utils.ColorInterp(baseColor, Color.White, 0.2f) : baseColor);
                }
                base.Update(left, top, right, bottom);
            }
        }

        protected List<Group> groups;

        static Sprite box, frame;
        public int scroll = 0;

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            box = Content.GetTexture("levelselectbase");
            frame = Content.GetTexture("frame");
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

        public void AddPack(ChartLoader.ChartGroup group)
        {
            Group g = new Group(100, (x) =>
            {
                bool temp = x.Expand;
                foreach (Group c in groups)
                {
                    if (c.Expand)
                    {
                        if (c.BottomEdge() < x.BottomEdge())
                        {
                            scroll += c.GetHeight();
                        }
                        c.Expand = false;
                    }
                }
                x.Expand = !temp;
                if (x.Expand)
                {
                    x.RecursivePopOutRooted();
                }
                scroll -= (int)x.BottomEdge() - ScreenUtils.ScreenHeight;
            }, () => { return false; }, group.label, "", Game.Options.Theme.SelectPack); //groups don't know when they're expanded :(
            foreach (CachedChart chart in group.charts)
            {
                g.AddItem(new Group(80, (x) =>
                {
                    Chart m = ChartLoader.Cache.LoadChart(chart);
                    Game.Gameplay.ChangeChart(m);
                }, () => { return false; }, chart.artist + " - " + chart.title, chart.diffname + " ("+chart.keymode.ToString()+"k)", Game.Options.Theme.SelectChart));
                /*g.AddItem(new Group(80, (x) =>
                {
                    bool temp = x.Expand;
                    foreach (Group c in g.Children)
                    {
                        if (c.Expand)
                        {
                            if (c.BottomEdge() < x.BottomEdge())
                            {
                                scroll += c.GetHeight();
                            }
                            c.Expand = false;
                        }
                    }
                    x.Expand = !temp;
                    if (x.Expand)
                    {
                        if (x.Children.Count == 0)
                        {
                            Chart m = ChartLoader.Cache.LoadChart(chart);
                            x.AddItem(new Group(80, (y) =>
                            {
                                Game.Gameplay.ChangeChart(m);
                            }, () => { return Game.CurrentChart.Data.SourcePath + Game.CurrentChart.Data.DiffName == m.Data.SourcePath + m.Data.DiffName; }, m.Data.DiffName, "", Game.Options.Theme.SelectDiff));
                        }
                        x.RecursivePopOutRooted();
                        scroll -= (int)x.BottomEdge() - ScreenUtils.ScreenHeight;
                    }
                }, () => { return Game.CurrentChart.Data.Title == chart.title; }, chart.title, chart.artist, Game.Options.Theme.SelectChart));*/
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
