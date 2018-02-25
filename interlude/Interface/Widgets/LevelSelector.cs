using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Beatmap;

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
            private ColorFade border;
            private ColorFade fill;

            public Group(int height, int width, Action<Group> action, Func<bool> highlight, string line1, string line2, Color c)
            {
                Children = new List<Group>();
                this.height = height;
                this.width = width;
                title = line1;
                subtitle = line2;
                OnClick = action;
                Highlight = highlight;
                border = new ColorFade(Color.White, Color.White);
                fill = new ColorFade(c, Color.White);
                RecursivePopOut(0);
            }

            public void UpdatePosition(float y)
            {
                A.Target(width - (float)Math.Pow((y-ScreenUtils.Height) / 50f, 2), y);
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

            public virtual int Height()
            {
                int r = height;
                if (Expand)
                {
                    foreach (Group g in Children)
                    {
                        r += g.Height();
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
                SpriteBatch.DrawTilingTexture(box, left, top, right, bottom, 400, 0, 0, fill);
                SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30, border);
                SpriteBatch.DrawTextToFill(title, left + 20, top + 20, left + width, bottom - 20, border);
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
                        x += g.Height();
                    }
                }
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ScreenUtils.MouseOver(left, top, right, bottom))
                {
                    A.Move(150, 0);
                    fill.Target = 0.2f;
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        OnClick(this);
                    }
                }
                else
                {
                    fill.Target = Highlight() ? 0.4f : 0;
                }
                border.Update();
                fill.Update();
                base.Update(left, top, right, bottom);
            }
        }

        protected List<Group> groups;
        private Screens.ScreenLevelSelect parent;

        static Sprite box, frame;
        public static int scroll = 0;

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            this.parent = parent;
            box = Content.LoadTextureFromAssets("levelselectbase");
            frame = Content.LoadTextureFromAssets("frame");
            groups = new List<Group>();
            foreach (ChartLoader.ChartPack p in ChartLoader.Cache)
            {
                AddPack(p);
            }
        }

        public void AddPack(ChartLoader.ChartPack pack)
        {
            int width = (int)(ScreenUtils.Width*0.8f - 150);
            Group g = new Group(100, width, (x) =>
            {
                bool temp = x.Expand;
                foreach (Group c in groups)
                {
                    if (c.Expand)
                    {
                        if (c.BottomEdge() < x.BottomEdge())
                        {
                            scroll += c.Height();
                        }
                        c.Expand = false;
                    }
                }
                x.Expand = !temp;
                if (x.Expand)
                {
                    x.RecursivePopOutRooted();
                }
                scroll -= (int)x.BottomEdge() - ScreenUtils.Height;
            }, () => { return ChartLoader.SelectedPack.title == pack.title; }, pack.title, "", Game.Options.Theme.SelectPack);
            foreach (ChartLoader.CachedChart chart in pack.charts)
            {
                g.AddItem(new Group(80, width, (x) =>
                {
                    bool temp = x.Expand;
                    foreach (Group c in g.Children)
                    {
                        if (c.Expand)
                        {
                            if (c.BottomEdge() < x.BottomEdge())
                            {
                                scroll += c.Height();
                            }
                            c.Expand = false;
                        }
                    }
                    x.Expand = !temp;
                    if (x.Expand)
                    {
                        if (x.Children.Count == 0)
                        {
                            MultiChart m = ChartLoader.LoadFromCache(chart);
                            foreach (Chart d in m.diffs)
                            {
                                x.AddItem(new Group(80, width, (y) =>
                                {
                                    Game.Instance.ChangeChart(d);
                                    parent.OnChangeChart();
                                    ChartLoader.SelectedChart = m;
                                    ChartLoader.SelectedPack = pack;
                                }, () => { return Game.CurrentChart.path + Game.CurrentChart.DifficultyName == d.path + d.DifficultyName; }, d.DifficultyName, "", Game.Options.Theme.SelectDiff));
                            }
                        }
                        x.RecursivePopOutRooted();
                        scroll -= (int)x.BottomEdge() - ScreenUtils.Height;
                    }
                }, () => { return ChartLoader.SelectedChart.header.title == chart.title; }, chart.title, chart.artist, Game.Options.Theme.SelectChart));
            }
            groups.Add(g);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            foreach (Group g in groups)
            {
                g.Draw(left, top, right, bottom);
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            int y = scroll;
            foreach (Group g in groups)
            {
                g.UpdatePosition(y);
                g.Update(left, top, right, bottom);
                y += g.Height();
            }
            if (Input.KeyPress(OpenTK.Input.Key.Up))
            {
                scroll += 15;
            }
            else if (Input.KeyPress(OpenTK.Input.Key.Down))
            {
                scroll -= 15;
            }
            scroll += Input.MouseScroll * 50;
        }
    }
}
