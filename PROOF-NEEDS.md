# PROOF-NEEDS.md
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

## Current State

- **LOC**: ~9,350
- **Languages**: Ada, Idris2, Zig, Nickel
- **Existing ABI proofs**: `src/abi/*.idr` (template-level)
- **Dangerous patterns**: None detected

## What Needs Proving

### DNS Record Manipulation (hinfo_loc_fluctuator_ada/src/)
- `dns_records.adb/ads`, `dns_records_extended.adb/ads` — DNS record creation and modification
- `dns_update.adb/ads` — DNS update protocol
- Prove: generated DNS records conform to RFC specifications (RFC 1035, etc.)
- Prove: DNS updates are idempotent or safely retryable

### Authentication (hinfo_loc_fluctuator_ada/src/secure_auth.adb)
- Secure authentication for DNS updates
- Prove: authentication tokens are never transmitted in cleartext
- Prove: authentication failure results in no DNS modifications

### Firewall Management (hinfo_loc_fluctuator_ada/src/firewall_manager.adb/ads)
- Firewall rule manipulation during DNS fluctuation
- Prove: firewall rules always return to a safe state after fluctuation

### Scheduler (hinfo_loc_fluctuator_ada/src/scheduler.adb/ads)
- Timed DNS fluctuation
- Prove: scheduler never produces overlapping fluctuation windows

### SDP Controller (hinfo_loc_fluctuator_ada/src/sdp_controller.adb/ads)
- SDP (Software-Defined Perimeter) control
- Prove: SDP state is consistent with DNS state after fluctuation

## Recommended Prover

- **SPARK** (natural fit — Ada code can be annotated with SPARK contracts)
- **Idris2** for ABI layer

## Priority

**HIGH** — Network security tool manipulating DNS records and firewall rules. Incorrect fluctuation could cause service outages or security exposures. SPARK annotations on the Ada code would be the most effective approach.
