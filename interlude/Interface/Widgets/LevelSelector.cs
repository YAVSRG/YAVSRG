using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Interface.Widgets
{
    public class LevelSelector : Widget
    {
        static Sprite box;

        protected class SelectableItem : Widget
        {
            public int Height;

            public SelectableItem(int height, int position)
            {
                Height = height;
                A = new AnchorPoint(800, position, AnchorType.MAX, AnchorType.MIN);
                B = new AnchorPoint(0, position + Height, AnchorType.MAX, AnchorType.MIN);
            }

            public void SetPosition(float x, float y) //x is width, y is position vertically
            {
                A.Target(x, y);
                B.Target(0, y + Height);
            }

            public virtual bool Match(object o)
            {
                return false;
            }

            public bool Update(float left, float top, float right, float bottom, LevelSelector parent)
            {
                bool flag = false;
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ScreenUtils.MouseOver(left, top, right, bottom))
                {
                    A.Move(150, 0);
                    if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                    {
                        flag = true;
                        OnClick(parent);
                    }
                }
                A.Update();
                B.Update();
                return flag;
            }

            public virtual void OnClick(LevelSelector parent) { }
        }

        protected class SelectPack : SelectableItem
        {
            private ChartLoader.ChartPack data;

            public SelectPack(ChartLoader.ChartPack pack, int position) : base(100, position)
            {
                data = pack;
            }

            public override void Draw(float left, float top, float right, float bottom)
            {
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                SpriteBatch.Draw(box, left, top, right, bottom, System.Drawing.Color.LightGray);
                SpriteBatch.DrawTextToFill(data.title, left + 20, top + 20, left+900, bottom - 20, System.Drawing.Color.White);
            }

            public override bool Match(object o)
            {
                return (o is ChartLoader.ChartPack) && ((ChartLoader.ChartPack)o).title == data.title;
            }

            public override void OnClick(LevelSelector parent)
            {
                parent.pack = data;
                parent.ExpandPack(data, (int)B.AbsY);
            }
        }

        protected class SelectChart : SelectableItem
        {
            private ChartLoader.CachedChart data;

            public SelectChart(ChartLoader.CachedChart map, int position) : base(80, position)
            {
                data = map;
            }

            public override void Draw(float left, float top, float right, float bottom)
            {
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                SpriteBatch.Draw(box, left, top, right, bottom, System.Drawing.Color.LightGreen);
                SpriteBatch.DrawTextToFill(data.title, left + 10, top + 10,left+900, bottom - 10, System.Drawing.Color.White);
            }

            public override void OnClick(LevelSelector parent)
            {
                parent.chart = ChartLoader.LoadFromCache(data);
                parent.ExpandChart(data, parent.chart.diffs, (int)B.AbsY);
            }

            public override bool Match(object o)
            {
                return (o is ChartLoader.CachedChart) && ((ChartLoader.CachedChart)o).title == data.title;
            }
        }

        protected class SelectDiff : SelectableItem
        {
            private Chart data;

            public SelectDiff(Chart diff, int position) : base(60, position)
            {
                data = diff;
            }

            public override void Draw(float left, float top, float right, float bottom)
            {
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                SpriteBatch.Draw(box, left, top, right, bottom, System.Drawing.Color.LightBlue);
                SpriteBatch.DrawTextToFill(data.DifficultyName, left + 10, top + 10, left + 900, bottom - 10, System.Drawing.Color.White);
            }

            public override void OnClick(LevelSelector parent)
            {
                Game.Instance.ChangeChart(data);
                parent.parent.OnChangeChart();
                ChartLoader.SelectedChart = parent.chart;
                ChartLoader.SelectedPack = parent.pack;
            }
        }

        protected List<SelectableItem> items;
        protected ChartLoader.ChartPack pack = ChartLoader.SelectedPack;
        protected MultiChart chart = ChartLoader.SelectedChart;
        private Screens.ScreenLevelSelect parent;

        public int scroll = 0;

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            this.parent = parent;
            box = Content.LoadTextureFromAssets("levelselectbase");
            items = new List<SelectableItem>();
        }

        public void AddPack(ChartLoader.ChartPack pack, int at)
        {
            items.Add(new SelectPack(pack,at));
        }

        public void ExpandPack(ChartLoader.ChartPack pack, int at)
        {
            int index = -1;
            List<int> remove = new List<int>();
            for (int i = 0; i < items.Count; i++)
            {
                if (items[i].Match(pack))
                {
                    index = i;
                }
                else if (items[i] is SelectChart || items[i] is SelectDiff)
                {
                    remove.Add(i);
                }
            }
            remove.Reverse();
            foreach (int i in remove)
            {
                items.RemoveAt(i);
                if (i < index) { index--; }
            }
            foreach (ChartLoader.CachedChart c in pack.charts)
            {
                items.Insert(index + 1, new SelectChart(c,at));
            }
        }

        public void ExpandChart(ChartLoader.CachedChart chart, List<Chart> diffs, int at)
        {
            int index = -1;
            List<int> remove = new List<int>();
            for (int i = 0; i < items.Count; i++)
            {
                if (items[i].Match(chart))
                {
                    index = i;
                }
                else if (items[i] is SelectDiff)
                {
                    remove.Add(i);
                }
            }
            remove.Reverse();
            foreach (int i in remove)
            {
                items.RemoveAt(i);
                if (i < index) { index--; }
            }
            foreach (Chart diff in diffs)
            {
                items.Insert(index + 1, new SelectDiff(diff,at));
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            int c = items.Count;
            for (int i = 0; i < c; i++)
            {
                items[i].Draw(left, top, right, bottom);
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            int c = items.Count;
            int y = scroll;
            for (int i = 0; i < c; i++)
            {
                items[i].SetPosition(800 - (float)Math.Pow(y / 50f, 2), y + ScreenUtils.Height);
                if (items[i].Update(left, top, right, bottom, this)) break;
                y += items[i].Height;
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
