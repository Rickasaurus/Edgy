namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Edgy.QuickGraph")>]
[<assembly: AssemblyProductAttribute("Edgy")>]
[<assembly: AssemblyDescriptionAttribute("A library to make working with graphs easy in F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
