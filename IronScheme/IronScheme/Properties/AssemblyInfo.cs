using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Permissions;

[assembly: AssemblyTitle("IronScheme")]
[assembly: AssemblyCompany("Llewellyn Pritchard")]
[assembly: AssemblyProduct("IronScheme")]
[assembly: AssemblyCopyright("Copyright © 2007-2015 Llewellyn Pritchard")]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]
[assembly: AssemblyInformationalVersion("1.0.0.0")]

// these are for when Oyster.IntX gets merged. ILMerge seems to forget about them.
[assembly: SecurityPermission(SecurityAction.RequestMinimum, SkipVerification = true)]
[module: UnverifiableCode]
