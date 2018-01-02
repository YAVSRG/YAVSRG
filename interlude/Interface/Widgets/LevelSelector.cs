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

        protected class SelectableItem
        {
            public int Height;

            public virtual void Draw (int x, int y) { }

            public virtual bool Match(object o)
            {
                return false;
            }

            public virtual void OnClick(LevelSelector parent) { }
        }

        protected class SelectPack : SelectableItem
        {
            private ChartLoader.ChartPack data;

            public SelectPack(ChartLoader.ChartPack pack)
            {
                data = pack;
                Height = 100;
            }

            public override void Draw(int x, int y)
            {
                SpriteBatch.Draw(box, x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightGray);
                //SpriteBatch.DrawRect(x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightGray);
                SpriteBatch.DrawText(data.title, 40f, x+20, y+20, System.Drawing.Color.White);
            }

            public override bool Match(object o)
            {
                return (o is ChartLoader.ChartPack) && ((ChartLoader.ChartPack)o).title == data.title;
            }

            public override void OnClick(LevelSelector parent)
            {
                parent.pack = data;
                parent.ExpandPack(data);
            }
        }

        protected class SelectChart : SelectableItem
        {
            private ChartLoader.CachedChart data;

            public SelectChart(ChartLoader.CachedChart map)
            {
                data = map;
                Height = 80;
            }

            public override void Draw(int x, int y)
            {
                x += ScreenUtils.Width / 8;
                SpriteBatch.Draw(box, x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightGreen);
                //SpriteBatch.DrawRect(x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightGreen);
                SpriteBatch.DrawText(data.title, 40f, x + 20, y + 10, System.Drawing.Color.White);
            }

            public override void OnClick(LevelSelector parent)
            {
                parent.chart = ChartLoader.LoadFromCache(data);
                parent.ExpandChart(data,parent.chart.diffs);
            }

            public override bool Match(object o)
            {
                return (o is ChartLoader.CachedChart) && ((ChartLoader.CachedChart)o).title == data.title;
            }
        }

        protected class SelectDiff : SelectableItem
        {
            private Chart data;

            public SelectDiff(Chart diff)
            {
                data = diff;
                Height = 60;
            }

            public override void Draw(int x, int y)
            {
                x += ScreenUtils.Width / 4;
                SpriteBatch.Draw(box, x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightBlue);
                //SpriteBatch.DrawRect(x, y, ScreenUtils.Width, y + Height, System.Drawing.Color.LightBlue);
                SpriteBatch.DrawText(data.DifficultyName, 40f, x + 20, y, System.Drawing.Color.White);
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

        public SlidingEffect scroll;

        public LevelSelector(Screens.ScreenLevelSelect parent) : base()
        {
            this.parent = parent;
            box = Content.LoadTextureFromAssets("levelselectbase");
            items = new List<SelectableItem>();
            scroll = new SlidingEffect(80);
        }

        public void AddPack(ChartLoader.ChartPack pack)
        {
            items.Add(new SelectPack(pack));
        }

        public void ExpandPack(ChartLoader.ChartPack pack)
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
                items.Insert(index+1, new SelectChart(c));
            }
        }

        public void ExpandChart(ChartLoader.CachedChart chart, List<Chart> diffs)
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
                items.Insert(index+1, new SelectDiff(diff));
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            int i = 0;
            int y = -ScreenUtils.Height + (int)scroll.Val;
            int c = items.Count;
            int x;
            while (i < c && y < ScreenUtils.Height)
            {
                x = ScreenUtils.Width - 900  + (int)Math.Pow(y/50f,2);
                items[i].Draw(x, y);
                y += items[i].Height;
                i++;
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            scroll.Update();
            int i = 0;
            int y = -ScreenUtils.Height + (int)scroll.Val;
            int c = items.Count;
            while (i < c && y < ScreenUtils.Height)
            {
                if (ScreenUtils.CheckButtonClick(0, y, ScreenUtils.Width, y + items[i].Height))
                {
                    items[i].OnClick(this);
                    scroll.Target -= y;
                    break;
                }
                y += items[i].Height;
                i++;
            }
            if (Input.KeyPress(OpenTK.Input.Key.Up))
            {
                scroll.Target += 15;
            }
            else if (Input.KeyPress(OpenTK.Input.Key.Down))
            {
                scroll.Target -= 15;
            }
        }
    }
}
