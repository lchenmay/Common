using System.Reflection.Metadata.Ecma335;

namespace UtilCs
{
    public class Cat
    {

        static string f1(int x)
        {
            return x.ToString();
        }
        static string f2(int x) => x.ToString();

        static Func<int, string> f3 = (x) => x.ToString();

    }

    public delegate int DelegateF1<in T>(T x);
}
