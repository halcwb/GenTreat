// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#load "paket-files/halcwb/GenBuild/scripts/targets.fsx"

open Fake
#if MONO
#else
open SourceLink
#endif


Target "All" DoNothing


"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "NUnit3"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"
  ==> "All"
  =?> ("ReleaseDocs",isLocalBuild)

"All"
#if MONO
#else
  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif
  ==> "NuGet"
  ==> "BuildPackage"

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"CleanDocs"
  ==> "GenerateHelpDebug"

"GenerateHelp"
  ==> "KeepRunning"

"ReleaseDocs"
  ==> "Release"

"BuildPackage"
  ==> "PublishNuget"
  ==> "Release"


RunTargetOrDefault "All"
