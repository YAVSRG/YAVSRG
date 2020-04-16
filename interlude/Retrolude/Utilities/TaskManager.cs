using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Threading;
using Prelude.Utilities;

namespace Interlude.Utilities
{
    public class TaskManager
    {
        public delegate bool UserTask(Action<string> Output);

        //represents a background task - it is labelled and some can be seen/manipulated by the user directly
        public class NamedTask
        {
            CancellationTokenSource _token = new CancellationTokenSource();
            Task _task;
            public string Name { get; private set; }
            public bool Visible { get; private set; }
            public string Progress { get; private set; }

            public NamedTask(UserTask task, string name, Action<bool> callback, bool visible)
            {
                Name = name;
                Visible = visible;
                Progress = "";
                _task = new Task(() => {
                    try
                    {
                        callback(task((v) => { if (!visible) Logging.Log(name + ": " + v, "", Logging.LogType.Info); Progress = v; }));
                        if (visible) Logging.Log("Completed task: " + name, "");
                    }
                    catch (Exception e)
                    {
                        Logging.Log("Exception occured in task " + name, e.ToString(), Logging.LogType.Error);
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
                if (_token.IsCancellationRequested) { Logging.Log("Tried to cancel " + Name + " but it has already been cancelled", "", Logging.LogType.Warning); return; }
                _token.Cancel();
                if (_task.Exception != null)
                {
                    Logging.Log("Exception occured in task " + Name, _task.Exception.ToString(), Logging.LogType.Error);
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

        public List<NamedTask> Tasks { get; private set; }

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
            if (Task.Visible) Logging.Log("Added task: " + Task.Name, "");
            Task.Start();
        }

        //schedules a task - track marks whether the user should be able to see it in the task list/view its progress/cancel it
        public NamedTask AddTask(UserTask Task, Action<bool> Callback, string Name, bool Track)
        {
            var t = new NamedTask(Task, Name, Callback, Track);
            AddTask(t);
            return t;
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
