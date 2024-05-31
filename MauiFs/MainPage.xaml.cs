namespace MauiFs
{
    using MauiFsLogics;
    using UtilMaui;

    public partial class MainPage : ContentPage
    {
        public MainPage()
        {
            InitializeComponent();

            Types.runtime.layout = this.layout;
            SPA.route(Types.runtime.layout, "/");
        }
    }

}
