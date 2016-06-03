namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenTreat.Lib")>]
[<assembly: AssemblyProductAttribute("Informedica.GenTreat.Lib")>]
[<assembly: AssemblyCompanyAttribute("halcwb")>]
[<assembly: AssemblyDescriptionAttribute("Domain library to model medical treatment")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
