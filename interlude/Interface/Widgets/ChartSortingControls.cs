using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Charts;
using static YAVSRG.Charts.ChartLoader;
using OpenTK;

namespace YAVSRG.Interface.Widgets
{
    public class ChartSortingControls : Widget
    {
        ScrollContainer sort;
        ScrollContainer group;
        Widget sortB, groupB;
        bool focus;
        Animations.AnimationColorMixer color;

        public ChartSortingControls() : base()
        {
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.HighlightColor));

            group = new ScrollContainer(20, 10, false, false) { State = 0 };
            sort = new ScrollContainer(20, 10, false, false) { State = 0 };
            AddChild(sortB = new SimpleButton("Sort by...", () => {
                sort.State = (sort.State + 1) % 2;
                sort.B.MoveTarget(0, sort.B.TargetY < 0 ? 300 : -300);
            }, () => { return false; }, 20f).PositionTopLeft(260, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(20, 10, AnchorType.MAX, AnchorType.MAX));

            sort.AddChild(SortButton("Difficulty", SortByDifficulty));
            sort.AddChild(SortButton("Artist", SortByArtist));
            sort.AddChild(SortButton("Creator", SortByCreator));
            sort.AddChild(SortButton("Title", SortByTitle));
            AddChild(sort.PositionTopLeft(260, 0, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(20,-300,AnchorType.MAX,AnchorType.MAX));

            AddChild(groupB = new SimpleButton("Group by...", () => {
                group.State = (group.State + 1) % 2;
                group.B.MoveTarget(0, group.B.TargetY < 0 ? 300 : -300);
            }, () => { return false; }, 20f).PositionTopLeft(520, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(280, 10, AnchorType.MAX, AnchorType.MAX));
            group.AddChild(GroupButton("Pack", GroupByPack));
            group.AddChild(GroupButton("Difficulty", GroupByDifficulty));
            group.AddChild(GroupButton("Artist", GroupByArtist));
            group.AddChild(GroupButton("Creator", GroupByCreator));
            group.AddChild(GroupButton("Title", GroupByTitle));
            group.AddChild(GroupButton("Keymode", GroupByKeymode));
            AddChild(group.PositionTopLeft(520, 0, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(280, -300, AnchorType.MAX, AnchorType.MAX));
            
        }

        private Widget SortButton(string l, Comparison<CachedChart> m)
        {
            return new SimpleButton(l, () => { SortMode = m; Refresh(); }, () => { return SortMode == m; }, 15f).PositionBottomRight(200,35,AnchorType.MIN,AnchorType.MIN);
        }

        private Widget GroupButton(string l, Func<CachedChart,string> m)
        {
            return new SimpleButton(l, () => { GroupMode = m; Refresh(); }, () => { return GroupMode == m; }, 15f).PositionBottomRight(200, 35, AnchorType.MIN, AnchorType.MIN);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            Game.Screens.DrawChartBackground(left, top, right, bottom, Utils.ColorInterp(Color.FromArgb(255,0,0,0), Game.Screens.BaseColor, 0.8f),1.5f);
            SpriteBatch.DrawRect(right - 520, top + 10, right - 20, top + 70, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawText(SearchString != "" ? SearchString : "Press tab to search...", 20f, right - 500, top + 22.5f, color);
            SpriteBatch.DrawFrame(right - 520, top + 10, right - 20, top + 70, 25f, color);
            SpriteBatch.DrawFrame(left-30, top, right+30, bottom, 30f, Game.Screens.HighlightColor);
            DrawWidgets(left, top, right, bottom);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            color.Target(focus ? Color.White : Game.Screens.HighlightColor);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (!focus && Input.KeyTap(OpenTK.Input.Key.Tab))
            {
                Input.ChangeIM(new InputMethod((s)=> { SearchString = s; },()=> { return SearchString; }, ()=> { Refresh(); }));
                focus = true;
            }
            else if (focus && Input.KeyTap(OpenTK.Input.Key.Tab, true))
            {
                Input.ChangeIM(null);
                focus = false;
            }
        }
    }
}
