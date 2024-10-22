#!/bin/bash
set -e

dotnet tool install -g dotnet-ilrepack

COMMON='-m -c Release --no-restore -tl:off -bl:package.binlog'

dotnet build $COMMON -t:Package $*