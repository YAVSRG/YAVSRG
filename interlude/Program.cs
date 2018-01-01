using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using YAVSRG.Interface;

namespace YAVSRG
{
    class Program
    {
        static void Main(string[] args)
        {
            Game g = new Game();
            try
            {
                g.Run(120.0);
            }
            catch (Exception e)
            {
                g.Exit();
                Application.Run(new CrashWindow(e.ToString()));
            }
            finally
            {
                g.Exit();
                g.Dispose();
            }

        }
    }
}
