namespace MauiFs
{
    using MauiFsLogics;
    using MauiFsLogics.Pages;

    public partial class MainPage : ContentPage
    {
        public MainPage()
        {
            InitializeComponent();

            Types.runtime.layout = this.layout;
            router(Types.runtime.layout, "/");
        }
    }

}
