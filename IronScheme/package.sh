#!/bin/bash
set -e

dotnet tool install -g dotnet-ilrepack

dotnet build --no-restore -tl:off -c Release -t:Package $*