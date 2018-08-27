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
            AddChild(new FramedButton("buttonbase", "Disconnect", Game.Multiplayer.Disconnect).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(hostButton = new FramedButton("buttonbase", "Host a lobby", Game.Multiplayer.HostLobby).PositionBottomRight(300, 100, AnchorType.MIN, AnchorType.MIN));
            AddChild(new TextEntryBox((s) => { lobbyCode = s; }, () => { return lobbyCode; }, () => { }, () => { Game.Multiplayer.JoinLobby(lobbyCode); }, () => { return "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to enter lobby code..."; })
     .PositionTopLeft(-250, 20, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(250, 80, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new BoolPicker("Play charts together", Game.Multiplayer.SyncCharts, (v) => { Game.Multiplayer.SyncCharts = v; }).PositionTopLeft(-50, 300, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(50, 350, AnchorType.CENTER, AnchorType.MIN));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (Game.Multiplayer.Hosting)
            {
                if (Game.Multiplayer.LobbyKey != "")
                {
                    SpriteBatch.Font1.DrawCentredText("Your lobby code is: " + Game.Multiplayer.LobbyKey, 30f, 0, top + 100, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font1.DrawCentredText("Press Ctrl+C to put it on the clipboard", 20f, 0, top + 150, Game.Options.Theme.MenuFont);
                }
                float y = top + 150;
                for (int i = 0; i < Game.Multiplayer.Clients.Length; i++)
                {
                    if (Game.Multiplayer.Clients[i]?.LoggedIn == true)
                    {
                        SpriteBatch.DrawRect(left + 10, y, left + 310, y + 40, i == Game.Multiplayer.Server.ChartPicker ? System.Drawing.Color.FromArgb(127, Game.Screens.BaseColor) : System.Drawing.Color.FromArgb(127, 0, 0, 0));
                        SpriteBatch.Font1.DrawText(Game.Multiplayer.Clients[i].Username, 25f, left + 20, y, Game.Options.Theme.MenuFont);
                        y += 50;
                    }
                }
                SpriteBatch.Font2.DrawParagraph("Chart picker is highlighted.\nLeft click to give someone chart picker role.\nRight click to kick someone.\nVery much WIP lol", 20f, -300, 0, 300, 600, Game.Options.Theme.MenuFont);
            }
            SpriteBatch.Font1.DrawJustifiedText("Multiplayer Lobby BETA", 30f, right - 10, bottom - 60, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (!Game.Multiplayer.Connected)
            {
                hostButton.State = 1;
            }
            else
            {
                hostButton.State = 0;
                if (Game.Multiplayer.Hosting)
                {
                    if (Input.KeyPress(OpenTK.Input.Key.ControlLeft) && Input.KeyTap(OpenTK.Input.Key.C))
                    {
                        System.Windows.Forms.Clipboard.SetText(Game.Multiplayer.LobbyKey);
                    }
                    float y = top + 150;
                    for (int i = 0; i < Game.Multiplayer.Clients.Length; i++)
                    {
                        if (Game.Multiplayer.Clients[i]?.LoggedIn == true)
                        {
                            if (ScreenUtils.MouseOver(left + 10, y, left + 310, y + 40))
                            {
                                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                                {
                                    Game.Multiplayer.Server.ChartPicker = i;
                                }
                                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                                {
                                    Game.Multiplayer.Server.Kick("Manually kicked by host", i);
                                }
                            }
                            y += 50;
                        }
                    }
                }
            }
        }
    }
}
