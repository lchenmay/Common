using System.Reflection.Metadata.Ecma335;

namespace UtilCs
{
    public class Option<T>
    {
        public enum OptionEnum 
        {
            Some,
            None
        }

        protected OptionEnum e = OptionEnum.None;

        public static void match(Option<T> o,Action<T> hSome,Action hNone) 
        {
            switch (o.e) 
            {
                case OptionEnum.Some:
                    Some<T> some = o as Some<T>;
                    hSome(some.val);
                    break;

                case OptionEnum.None:
                    hNone();
                    break;
            }
        }
    }

    public class Some<T> : Option<T> 
    {
        public Some(T val) {
            this.e = Option<T>.OptionEnum.Some;
            this.val = val;
        }

        public T val;
    }

    public class None<T> : Option<T>
    {
        public None()
        {
            this.e = Option<T>.OptionEnum.None;
        }

    }



}
