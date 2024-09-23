#!/bin/bash
dotnet build -v:m -tl:off -c Release -p:NoWarn="NETSDK1138%3bNU1702%3bNU1902%3bNU1903%3bNU1701%3bCS3021%3bSYSLIB0050" -p:IronSchemeBuildToolsFrameworkTarget=net9.0