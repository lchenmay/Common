namespace MauiFs
{
    using MauiFsLogics;

    public partial class MainPage : ContentPage
    {
        public MainPage()
        {
            InitializeComponent();

            Types.runtime.layout = this.layout;
            Pages.routing(Types.runtime.layout, "1");
        }
    }

}
