#!/bin/bash
set -e

dotnet tool install -g dotnet-ilrepack

COMMON='-m -c Release --no-restore -tl:off'

dotnet build $COMMON -t:Package $*