# HINFO-LOC Fluctuator Roadmap

**Last Updated**: 2025-12-17
**Current Version**: 0.1.0 (development)
**Overall Completion**: 40%

## Current State Summary

| Component | Status | Completion |
|-----------|--------|------------|
| RSR Compliance | Complete | 100% |
| CI Security (SHA-pinned) | Complete | 100% |
| Nix Fallback (flake.nix) | Complete | 100% |
| Ada Implementation | In Progress | 60% |
| Crypto Integration | Planned | 0% |
| DNS Integration | Planned | 0% |

## Phase 1: Foundation (Current)

### Completed
- [x] RSR (Rhodium Standard Repository) compliance
- [x] SHA-pinned GitHub Actions (supply chain security)
- [x] Guix package definition (guix.scm)
- [x] Nix flake fallback (flake.nix)
- [x] Core Ada type-safe DNS record structures
- [x] Authentication module with session management
- [x] Randomization engine framework
- [x] Text-based user interface
- [x] Zone file writer (BIND format)
- [x] Thread-safe audit logging
- [x] Scheduled fluctuation with Ada tasking
- [x] DNS UPDATE framework (RFC 2136)
- [x] Enterprise features (Firewall, SDP, Security Headers, Protocol Management)
- [x] Comprehensive documentation

### In Progress
- [ ] Complete CSV parser in `randomizer.adb`
- [ ] Verify Ada build in CI pipeline

## Phase 2: Security Hardening (Q1 2026)

**Priority**: CRITICAL

### Crypto Integration
- [ ] Integrate bcrypt/Argon2 password hashing library
- [ ] Replace demo credentials in `secure_auth.adb`
- [ ] Implement TSIG authentication (RFC 2845)
- [ ] Add SPA packet encryption/decryption (AES-256-GCM, ChaCha20-Poly1305)

### Testing
- [ ] Unit test framework (target: 70%+ coverage)
- [ ] Integration tests with mock DNS server
- [ ] Security-focused test cases

### CI/CD Improvements
- [ ] Add Ada syntax/build verification to CI
- [ ] Implement SPARK verification step (optional)

## Phase 3: DNS Integration (Q2 2026)

**Priority**: HIGH

### Real Server Integration
- [ ] Test with BIND 9
- [ ] Test with PowerDNS
- [ ] Test with NSD
- [ ] Document server-specific configuration

### DNS UPDATE Protocol
- [ ] Complete RFC 2136 implementation
- [ ] TSIG signature generation/verification
- [ ] Zone transfer (AXFR) support
- [ ] Incremental zone transfer (IXFR) support

### Firewall Integration
- [ ] Test actual firewall command execution
- [ ] Add dry-run mode for safety
- [ ] Support for firewalld, iptables, nftables, pf

## Phase 4: Formal Verification (Q3 2026)

**Priority**: MEDIUM

### SPARK Verification
- [ ] Add SPARK annotations to `secure_auth.ads/adb`
- [ ] Add SPARK annotations to `sdp_controller.ads/adb`
- [ ] Prove absence of runtime errors
- [ ] Prove security properties

### Certification
- [ ] Document verification results
- [ ] Achieve Platinum Tier RSR compliance (90%+)

## Phase 5: Production Release (Q4 2026)

**Priority**: MEDIUM

### Release Preparation
- [ ] Complete security audit
- [ ] Performance optimization
- [ ] Production deployment guide
- [ ] Operational runbooks

### Distribution
- [ ] Submit to Guix channel
- [ ] Nixpkgs submission
- [ ] Container images (if requested)

---

## Known Security Issues

### HIGH Priority

1. **Demo Credentials** (`secure_auth.adb:17-38`)
   - Current: Hardcoded SHA256 demo hashes
   - Required: bcrypt/Argon2 integration
   - Risk: Authentication bypass in production

2. **Password Bypass** (`secure_auth.adb:126-131`)
   - Current: Any non-empty password accepted in demo mode
   - Required: Proper password verification
   - Risk: Authentication bypass

### MEDIUM Priority

3. **No Real Cryptography**
   - Affects: TSIG, SPA, password hashing
   - Required: Integration with crypto library (libsodium, AWS-LC, or similar)

4. **DNS Server Integration Untested**
   - Affects: All DNS UPDATE operations
   - Required: Integration testing with real DNS servers

---

## Architecture Notes

### Design Principles
1. **Memory Safety**: Ada's ownership model prevents entire classes of bugs
2. **Type Safety**: Compile-time bounds checking on all coordinates/TTLs
3. **Constant-Time Operations**: Authentication uses constant-time comparison
4. **Defense in Depth**: Multiple layers of security checks

### Key Files
- `src/dns_records.ads` - Core type definitions
- `src/secure_auth.adb` - Authentication (security-critical)
- `src/sdp_controller.ads` - Zero-trust SDP
- `src/firewall_manager.ads` - Firewall integration

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Priority Areas for Contributors**:
1. Crypto library integration (bcrypt/Argon2)
2. Unit test coverage
3. DNS server integration testing
4. SPARK annotations

---

## References

- [RFC 1034](https://tools.ietf.org/html/rfc1034): DNS Concepts
- [RFC 1035](https://tools.ietf.org/html/rfc1035): DNS Implementation
- [RFC 1876](https://tools.ietf.org/html/rfc1876): LOC Record Format
- [RFC 2136](https://tools.ietf.org/html/rfc2136): DNS UPDATE
- [RFC 2845](https://tools.ietf.org/html/rfc2845): TSIG Authentication
- [RFC 8482](https://tools.ietf.org/html/rfc8482): HINFO/LOC Deprecation
- [NIST SP 800-207](https://csrc.nist.gov/publications/detail/sp/800-207/final): Zero Trust Architecture
