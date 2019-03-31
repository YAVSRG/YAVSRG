using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Prelude.Utilities
{
    //simplistic logging system for game
    public class Logging
    {
        //attach your log handling here
        public static event Action<string, string, LogType> OnLog;

        static FileStream LogFile = new FileStream("log.txt", FileMode.Append);
        static StreamWriter LogFileWriter = new StreamWriter(LogFile);

        //enum to tag logged messages with
        public enum LogType
        {
            Debug,
            Info,
            Warning,
            Error,
            Critical
        }

        //call this method to log anything
        //main is the main event e.g for errors, describe where the error occured
        //details is for additional info - in the main game this is not displayed on the toolbar or in the log so this is for extra detail the user doesnt need
        public static void Log(string Main, string Details = "", LogType Type = LogType.Info)
        {
            string s = "[" + Type.ToString() + "] " + Main + (Details == "" ? "" : ": " + Details);
            LogFileWriter.WriteLine(s); //writes formatted string to log file
            OnLog?.Invoke(Main, Details, Type); //then runs whatever callbacks have been attached
        }

        //call this method when the program is closing to release the log file
        public static void Close()
        {
            LogFileWriter.Close();
            LogFile.Close();
        }
    }
}
