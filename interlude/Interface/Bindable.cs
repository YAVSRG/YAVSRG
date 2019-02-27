using System;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;

namespace YAVSRG.Interface
{
    public class Bindable<T>
    {
        protected T _value;
        protected Action OnChanged;
        public virtual T Value
        {
            get { return _value; }
            set
            {
                _value = value;
                OnChanged?.Invoke();
            }
        }

        public static implicit operator T(Bindable<T> obj)
        {
            return obj._value;
        }

        public void SetOnChangedAction(Action a)
        {
            OnChanged = a;
        }
    }

    public class BindableFloat : Bindable<float>
    {
        public float Min { get; private set; }
        public float Max { get; private set; }
        public override float Value
        {
            get { return _value; }
            set
            {
                value = Math.Max(Math.Min(value,Max),Min);
                if (value != _value)
                {
                    _value = value;
                    OnChanged?.Invoke();
                }
            }
        }

        public BindableFloat(float Min, float Max)
        {
            this.Min = Min;
            this.Max = Max;
        }
    }

    public class BindableInt : Bindable<int>
    {
        public int Min { get; private set; }
        public int Max { get; private set; }
        public override int Value
        {
            get { return _value; }
            set
            {
                value = Math.Max(Math.Min(value, Max), Min);
                if (value != _value)
                {
                    _value = value;
                    OnChanged?.Invoke();
                }
            }
        }

        public BindableInt(int Min, int Max)
        {
            this.Min = Min;
            this.Max = Max;
        }
    }

    public class BindableConverter<T> : JsonConverter
    {
        private T _defaultValue;
        private T _min, _max;

        public BindableConverter(T Default) : this(Default, default(T), default(T))
        {
        }

        public BindableConverter(T Default, T Min, T Max)
        {
            _defaultValue = Default;
            _min = Min;
            _max = Max;
        }

        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            JValue value = (JValue)JToken.Load(reader);
            T Value;
            try
            {
                Value = (T)Convert.ChangeType(value.Value, typeof(T));
            }
            catch (Exception e)
            {
                Utilities.Logging.Log(e.ToString(), "");
                Value = _defaultValue;
            }
            if (objectType == typeof(BindableFloat))
            {
                return new BindableFloat((float)(object)_min, (float)(object)_max) { Value = (float)(object)Value };
            }
            else if (objectType == typeof(BindableInt))
            {
                return new BindableInt((int)(object)_min, (int)(object)_max) { Value = (int)(object)Value };
            }
            else
            {
                return new Bindable<T>() { Value = Value };
            }
        }

        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, ((Bindable<T>)value).Value);
        }

        public override bool CanConvert(Type objectType)
        {
            return typeof(Bindable<T>) == objectType;
        }
    }
}

