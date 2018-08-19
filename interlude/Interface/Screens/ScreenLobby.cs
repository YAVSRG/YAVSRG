using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    class ScreenLobby : Screen
    {
        Widget hostButton;
        string lobbyCode = "";

        public ScreenLobby()
        {
            AddChild(new FramedButton("buttonbase", "Close lobby", Game.Multiplayer.CloseLobby).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(hostButton = new FramedButton("buttonbase", "Host a lobby", Game.Multiplayer.HostLobby).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(new TextEntryBox((s) => { lobbyCode = s; }, () => { return lobbyCode; }, () => { }, () => { Game.Multiplayer.JoinLobby(lobbyCode); }, () => { return "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to enter lobby code..."; })
     .PositionTopLeft(-250, 20, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(250, 80, AnchorType.CENTER, AnchorType.MIN));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (Game.Multiplayer.Hosting)
            {
                SpriteBatch.Font1.DrawCentredText("Your lobby code is: " + Game.Multiplayer.LobbyKey, 30f, 0, top + 100, Game.Options.Theme.MenuFont);
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (!Game.Multiplayer.Hosting)
            {
                hostButton.State = 1;
            }
            else
            {
                hostButton.State = 0;
            }
        }
    }
}
