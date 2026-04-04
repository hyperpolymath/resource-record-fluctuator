#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# bench_dns.sh — Micro-benchmarks for resource-record-fluctuator operations.
# Measures performance of DNS record generation (shell-level proxy benchmark).

set -euo pipefail

BASE=/var/mnt/eclipse/repos/resource-record-fluctuator

echo "=== DNS Record Benchmark ==="

# Benchmark: File system operations (proxy for record I/O speed)
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Benchmark 1: Record file creation throughput
echo "--- Benchmark: Zone file creation ---"
START=$(date +%s%N)
for i in $(seq 1 1000); do
  echo "host${i}.example.com IN HINFO x86_64 Linux" > "$TMPDIR/record_${i}.txt"
done
END=$(date +%s%N)
ELAPSED=$(( (END - START) / 1000000 ))
echo "1000 zone records created in ${ELAPSED}ms"
echo "Rate: $(( 1000 * 1000 / (ELAPSED + 1) )) records/sec"

# Benchmark 2: Record parsing throughput
echo ""
echo "--- Benchmark: Zone file parsing ---"
START=$(date +%s%N)
COUNT=0
for f in "$TMPDIR"/*.txt; do
  domain=$(grep -o "^[^ ]*" "$f")
  COUNT=$((COUNT + 1))
done
END=$(date +%s%N)
ELAPSED=$(( (END - START) / 1000000 ))
echo "$COUNT records parsed in ${ELAPSED}ms"

# Benchmark 3: LOC coordinate generation
echo ""
echo "--- Benchmark: LOC record creation ---"
START=$(date +%s%N)
for i in $(seq 1 500); do
  printf "geo%d.example.com IN LOC %d %d %d %.1fm 1m 10000m 10m\n" \
    $i $(( i % 90 )) $(( i % 180 )) 0 50.0 >> "$TMPDIR/loc_records.txt"
done
END=$(date +%s%N)
ELAPSED=$(( (END - START) / 1000000 ))
echo "500 LOC records created in ${ELAPSED}ms"

echo ""
echo "=== Benchmark complete ==="
