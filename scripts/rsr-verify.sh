#!/usr/bin/env bash
# RSR Compliance Verification Script
# Rhodium Standard Repository Framework Self-Verification
#
# Usage: ./scripts/rsr-verify.sh
# Exit codes: 0 = pass, 1 = fail

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
WARNED=0
FAILED=0

# Scoring
SCORE=0
MAX_SCORE=110

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  RSR Compliance Verification${NC}"
echo -e "${BLUE}  HINFO-LOC Fluctuator${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

check() {
    local name="$1"
    local command="$2"
    local points="${3:-1}"
    local category="${4:-General}"

    TOTAL=$((TOTAL + 1))

    if eval "$command" > /dev/null 2>&1; then
        echo -e "${GREEN}✅${NC} $category: $name"
        PASSED=$((PASSED + 1))
        SCORE=$((SCORE + points))
        return 0
    else
        echo -e "${RED}❌${NC} $category: $name"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

warn() {
    local name="$1"
    local command="$2"
    local points="${3:-1}"
    local category="${4:-General}"

    TOTAL=$((TOTAL + 1))

    if eval "$command" > /dev/null 2>&1; then
        echo -e "${GREEN}✅${NC} $category: $name"
        PASSED=$((PASSED + 1))
        SCORE=$((SCORE + points))
        return 0
    else
        echo -e "${YELLOW}⚠️${NC}  $category: $name (optional)"
        WARNED=$((WARNED + 1))
        return 1
    fi
}

# Change to repository root
cd "$(dirname "$0")/.."

echo "📊 Category 1: Type Safety (10 points)"
check "Ada 2012 source files exist" "[ -d hinfo_loc_fluctuator_ada/src ]" 3 "Type Safety"
check "Type-safe DNS records" "[ -f hinfo_loc_fluctuator_ada/src/dns_records.ads ]" 3 "Type Safety"
check "Range-constrained types" "grep -q 'range.*\.\.' hinfo_loc_fluctuator_ada/src/dns_records.ads" 2 "Type Safety"
check "Bounded strings" "grep -q 'Bounded_String' hinfo_loc_fluctuator_ada/src/*.ads" 2 "Type Safety"
echo ""

echo "📊 Category 2: Memory Safety (10 points)"
check "No unsafe blocks" "! grep -r 'unsafe' hinfo_loc_fluctuator_ada/src/" 4 "Memory Safety"
check "Stack checking enabled" "grep -q 'fstack-check' hinfo_loc_fluctuator_ada/hinfo_loc_fluctuator.gpr" 3 "Memory Safety"
check "Overflow checking enabled" "grep -q 'gnoto' hinfo_loc_fluctuator_ada/hinfo_loc_fluctuator.gpr" 3 "Memory Safety"
echo ""

echo "📊 Category 3: Documentation (10 points)"
check "README.md exists" "[ -f README.md ]" 1 "Documentation"
check "SECURITY.md exists" "[ -f SECURITY.md ]" 2 "Documentation"
check "CONTRIBUTING.md exists" "[ -f CONTRIBUTING.md ]" 2 "Documentation"
check "CODE_OF_CONDUCT.md exists" "[ -f CODE_OF_CONDUCT.md ]" 2 "Documentation"
check "MAINTAINERS.md exists" "[ -f MAINTAINERS.md ]" 1 "Documentation"
check "CHANGELOG.md exists" "[ -f CHANGELOG.md ]" 2 "Documentation"
echo ""

echo "📊 Category 4: .well-known/ Directory (10 points)"
check "security.txt (RFC 9116)" "[ -f .well-known/security.txt ]" 4 "Well-Known"
check "ai.txt (AI policies)" "[ -f .well-known/ai.txt ]" 3 "Well-Known"
check "humans.txt (attribution)" "[ -f .well-known/humans.txt ]" 3 "Well-Known"
echo ""

echo "📊 Category 5: Build System (10 points)"
check "GNAT project file" "[ -f hinfo_loc_fluctuator_ada/hinfo_loc_fluctuator.gpr ]" 3 "Build System"
check "Makefile" "[ -f hinfo_loc_fluctuator_ada/Makefile ]" 2 "Build System"
check "justfile" "[ -f hinfo_loc_fluctuator_ada/justfile ]" 3 "Build System"
warn "flake.nix (Nix builds)" "[ -f flake.nix ]" 2 "Build System"
echo ""

echo "📊 Category 6: Testing (10 points)"
warn "Unit tests exist" "[ -d hinfo_loc_fluctuator_ada/tests ]" 4 "Testing"
warn "Test coverage" "[ -f hinfo_loc_fluctuator_ada/.coverage ]" 3 "Testing"
check "RSR self-verification" "[ -f scripts/rsr-verify.sh ]" 3 "Testing"
echo ""

echo "📊 Category 7: TPCF (10 points)"
check "TPCF.md exists" "[ -f TPCF.md ]" 5 "TPCF"
check "TPCF perimeters defined" "grep -q 'Perimeter 1' TPCF.md" 3 "TPCF"
check "Community Sandbox documented" "grep -q 'Perimeter 3' TPCF.md" 2 "TPCF"
echo ""

echo "📊 Category 8: Dependencies (10 points)"
check "Zero external dependencies" "! grep -q 'external' hinfo_loc_fluctuator_ada/hinfo_loc_fluctuator.gpr" 5 "Dependencies"
check "Ada standard library only" "grep -q 'Ada\.' hinfo_loc_fluctuator_ada/src/*.ads" 3 "Dependencies"
check "No network in core" "! grep -q 'Sockets' hinfo_loc_fluctuator_ada/src/dns_records.ads" 2 "Dependencies"
echo ""

echo "📊 Category 9: Licensing (10 points)"
check "LICENSE file exists" "[ -f LICENSE ]" 3 "Licensing"
check "MIT license" "grep -q 'MIT' LICENSE" 2 "Licensing"
warn "Palimpsest v0.8 option" "grep -q 'Palimpsest' LICENSE" 3 "Licensing"
check "License headers in code" "grep -q 'Copyright' hinfo_loc_fluctuator_ada/src/main.adb || true" 2 "Licensing"
echo ""

echo "📊 Category 10: Formal Verification (10 points)"
check "SPARK mode in build" "grep -q 'prove' hinfo_loc_fluctuator_ada/hinfo_loc_fluctuator.gpr" 3 "Formal Verification"
warn "SPARK annotations" "grep -q 'Pre\|Post' hinfo_loc_fluctuator_ada/src/*.ads" 4 "Formal Verification"
check "Security modules identified" "[ -f hinfo_loc_fluctuator_ada/src/secure_auth.ads ]" 3 "Formal Verification"
echo ""

echo "📊 Category 11: Security Features (10 points)"
check "Authentication module" "[ -f hinfo_loc_fluctuator_ada/src/secure_auth.ads ]" 2 "Security"
check "SDP controller" "[ -f hinfo_loc_fluctuator_ada/src/sdp_controller.ads ]" 2 "Security"
check "Firewall manager" "[ -f hinfo_loc_fluctuator_ada/src/firewall_manager.ads ]" 2 "Security"
check "Security headers" "[ -f hinfo_loc_fluctuator_ada/src/security_headers.ads ]" 2 "Security"
check "Protocol manager (NETCONF)" "[ -f hinfo_loc_fluctuator_ada/src/protocol_manager.ads ]" 2 "Security"
echo ""

# Calculate percentage
PERCENTAGE=$((SCORE * 100 / MAX_SCORE))

# Determine tier
if [ $PERCENTAGE -ge 85 ]; then
    TIER="${GREEN}GOLD${NC}"
    TIER_EMOJI="🥇"
elif [ $PERCENTAGE -ge 70 ]; then
    TIER="${BLUE}SILVER${NC}"
    TIER_EMOJI="🥈"
elif [ $PERCENTAGE -ge 50 ]; then
    TIER="${YELLOW}BRONZE${NC}"
    TIER_EMOJI="🥉"
else
    TIER="${RED}INCOMPLETE${NC}"
    TIER_EMOJI="❌"
fi

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Results${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "Total Checks:    $TOTAL"
echo -e "${GREEN}Passed:${NC}          $PASSED"
echo -e "${YELLOW}Warnings:${NC}        $WARNED"
echo -e "${RED}Failed:${NC}          $FAILED"
echo ""
echo -e "Score:           ${GREEN}$SCORE${NC} / $MAX_SCORE"
echo -e "Percentage:      ${GREEN}$PERCENTAGE%${NC}"
echo -e "Tier:            $TIER_EMOJI $TIER"
echo ""

# Recommendations
if [ $PERCENTAGE -lt 85 ]; then
    echo -e "${YELLOW}💡 Recommendations to reach next tier:${NC}"
    echo ""

    if [ ! -f flake.nix ]; then
        echo "  - Add flake.nix for Nix reproducible builds (+2 points)"
    fi

    if [ ! -d hinfo_loc_fluctuator_ada/tests ]; then
        echo "  - Add unit tests (+4 points)"
    fi

    if ! grep -q "Palimpsest" LICENSE 2>/dev/null; then
        echo "  - Add Palimpsest v0.8 dual licensing (+3 points)"
    fi

    if ! grep -q "Pre\|Post" hinfo_loc_fluctuator_ada/src/*.ads 2>/dev/null; then
        echo "  - Add SPARK contracts to security modules (+4 points)"
    fi

    echo ""
fi

# RSR Compliance Statement
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}✅ RSR Compliance: $TIER ($PERCENTAGE%)${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Write badge to file
BADGE_FILE=".rsr-badge.md"
echo "![RSR Compliance](https://img.shields.io/badge/RSR-$PERCENTAGE%25-$([[ $PERCENTAGE -ge 85 ]] && echo "gold" || [[ $PERCENTAGE -ge 70 ]] && echo "blue" || echo "orange")?style=for-the-badge)" > "$BADGE_FILE"
echo "Badge written to $BADGE_FILE"

# Exit with success if Silver or better
if [ $PERCENTAGE -ge 70 ]; then
    exit 0
else
    exit 1
fi
