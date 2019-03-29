using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Prelude.Utilities
{
    public class Logging
    {
        public static event Action<string, string, LogType> OnLog;
        static FileStream LogFile = new FileStream("log.txt", FileMode.Append);
        static StreamWriter LogFileWriter = new StreamWriter(LogFile);

        public enum LogType
        {
            Debug,
            Info,
            Warning,
            Error,
            Critical
        }

        public static void Log(string Main, string Details = "", LogType Type = LogType.Info)
        {
            string s = "[" + Type.ToString() + "] " + Main + (Details == "" ? "" : ": " + Details);
            LogFileWriter.WriteLine(s);
            OnLog?.Invoke(Main, Details, Type);
        }

        public static void Close()
        {
            LogFileWriter.Close();
            LogFile.Close();
        }
    }
}
