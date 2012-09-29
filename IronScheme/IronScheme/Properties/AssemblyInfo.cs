using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Permissions;

[assembly: AssemblyTitle("IronScheme")]
[assembly: AssemblyCompany("leppie")]
[assembly: AssemblyProduct("IronScheme")]
[assembly: AssemblyCopyright("Copyright © leppie 2007,2008,2009,2010,2011,2012")]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

[assembly:AllowPartiallyTrustedCallers]

// these are for when Oyster.IntX gets merged. ILMerge seems to forget about them.
[assembly: SecurityPermission(SecurityAction.RequestMinimum, SkipVerification = true)]
[module: UnverifiableCode]



