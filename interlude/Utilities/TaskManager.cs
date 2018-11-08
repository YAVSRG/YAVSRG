using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading;

namespace YAVSRG.Utilities
{
    public class TaskManager
    {
        public delegate bool UserTask(Action<string> Output);

        //represents a background task - it is labelled and some can be seen/manipulated by the user directly
        public class NamedTask
        {
            CancellationTokenSource _token = new CancellationTokenSource();
            Task _task;
            public readonly string Name;
            public readonly bool Track;
            public string Progress { get; private set; }

            public NamedTask(UserTask Task, string Name, Action<bool> Callback, bool Track)
            {
                this.Name = Name;
                this.Track = Track;
                Progress = "";
                _task = new Task(() => {
                    try
                    {
                        Callback(Task((v) => { Progress = v; }));
                        if (Track) Logging.Log("Completed task: " + Name);
                    }
                    catch (Exception e)
                    {
                        Logging.Log("Exception occured in task " + Name + ": " + e.ToString());
                    }
                    finally
                    {
                        Dispose();
                    }
                }, _token.Token);
            }

            public void Start()
            {
                _task.Start();
            }

            public void Cancel()
            {
                if (_token.IsCancellationRequested) { Logging.Log("Tried to cancel " + Name + " but it has already been cancelled", Logging.LogType.Warning); return; }
                _token.Cancel();
                if (_task.Exception != null)
                {
                    Logging.Log("Exception in task " + Name + ": " + _task.Exception.ToString());
                }
            }

            private void Dispose()
            {
                lock (Game.Tasks.Tasks)
                {
                    Game.Tasks.Tasks.Remove(this);
                } 
            }

            public TaskStatus Status
            {
                get { return _task.Status; }
            }
        }

        public List<NamedTask> Tasks;

        public TaskManager()
        {
            Tasks = new List<NamedTask>();
        }

        public void AddTask(NamedTask Task)
        {
            lock (Tasks)
            {
                Tasks.Add(Task);
            }
            if (Task.Track) Game.Screens.Toolbar.Chat.AddLine("Tasks", "Added task: " + Task.Name, true);
            Task.Start();
        }

        //schedules a task - track marks whether the user should be able to see it in the task list/view its progress/cancel it
        public void AddTask(UserTask Task, Action<bool> Callback, string Name, bool Track)
        {
            AddTask(new NamedTask(Task, Name, Callback, Track));
        }

        public void StopAll()
        {
            lock (Tasks)
            {
                foreach (NamedTask t in Tasks)
                {
                    t.Cancel();
                }
            }
        }

        public bool HasTasksRunning()
        {
            lock (Tasks)
            {
                foreach
                    (NamedTask t in Tasks)
                {
                    if (t.Status == TaskStatus.Running)
                    {
                        return true;
                    }
                }
            }
            return false;
        }
    }
}
