#!/usr/bin/env bash
set -eo pipefail

dotnet build -c Release csharp-benchmarks/CsharpBenchmarks.csproj
dotnet csharp-benchmarks/bin/Release/net8.0/CsharpBenchmarks.dll "$@"
