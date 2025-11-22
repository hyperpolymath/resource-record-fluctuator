# Security Policy

## Supported Versions

We currently support the following versions with security updates:

| Version | Supported          | Notes |
| ------- | ------------------ | ----- |
| 2.x.x   | :white_check_mark: | Current development (enterprise features) |
| 1.x.x   | :white_check_mark: | Basic HINFO/LOC fluctuation |
| < 1.0   | :x:                | Prototype/experimental only |

## Reporting a Vulnerability

**DO NOT** open public GitHub issues for security vulnerabilities.

### Preferred Reporting Methods

1. **GitHub Security Advisories** (Recommended)
   - Navigate to https://github.com/Hyperpolymath/resource-record-fluctuator/security/advisories/new
   - Provides private communication channel
   - Supports CVE assignment

2. **Email**
   - Send to: security@example.com (replace with actual contact)
   - Include "HINFO-LOC SECURITY" in subject line
   - PGP key: https://github.com/Hyperpolymath.gpg (if available)

3. **security.txt**
   - See `.well-known/security.txt` for canonical contact information
   - Compliant with RFC 9116

### What to Include

Please provide:

- **Description**: What is the vulnerability?
- **Impact**: What can an attacker do?
- **Affected Component**: Which module/file?
- **Reproduction**: Steps to reproduce
- **Proposed Fix**: If you have one (optional)
- **CVE Request**: Do you want a CVE assigned?

### Response Timeline

We aim to:

- **Acknowledge** your report within **48 hours**
- **Confirm** the vulnerability within **7 days**
- **Develop a fix** within **14-30 days** (depending on severity)
- **Publish advisory** after fix is deployed or 90 days (whichever is sooner)

### Severity Levels

| Severity | Response Time | Examples |
|----------|---------------|----------|
| **Critical** | 24 hours | Remote code execution, privilege escalation |
| **High** | 7 days | Authentication bypass, data exfiltration |
| **Medium** | 14 days | DoS, information disclosure |
| **Low** | 30 days | Minor issues, hardening opportunities |

## Security Features

This project implements multiple layers of security:

### 1. Type Safety (Ada 2012)

**Protection Against:**
- Buffer overflows
- Integer overflows
- Type confusion
- Null pointer dereferences

**Implementation:**
```ada
type Latitude_Degrees is delta 0.000001 range -90.0 .. 90.0;
type TTL_Seconds is range 0 .. 2_147_483_647;
```

**Compiler Flags (Always On):**
- `-gnato`: Overflow checking
- `-fstack-check`: Stack overflow protection
- `-gnatVa`: Validity checking
- `-gnatwe`: Warnings as errors

### 2. Memory Safety

**Protection Against:**
- Use-after-free
- Double-free
- Memory leaks
- Data races

**Implementation:**
- Ada ownership model (no manual memory management)
- Protected types for thread-safe access
- Zero `unsafe` blocks in entire codebase

### 3. Authentication & Authorization

**Module**: `secure_auth.ads/adb`

**Features:**
- Permission levels (None, Read-Only, Modify-Local, Modify-Remote, Admin)
- Session timeout (30 minutes default)
- Constant-time password comparison (timing attack prevention)
- Replay attack detection (nonce-based)

**⚠️ WARNING**: Demo credentials exist for development. **MUST** be changed before deployment.

### 4. Zero-Trust Architecture

**Module**: `sdp_controller.ads/adb`

**Features:**
- Software-Defined Perimeter (SDP)
- Single Packet Authorization (SPA)
- Continuous verification
- Device posture validation
- Multi-factor authentication support

**Standards:**
- NIST SP 800-207 (Zero Trust Architecture)
- CSA SDP Specification v2.0

### 5. Firewall Integration

**Module**: `firewall_manager.ads/adb`

**Features:**
- Dynamic port rotation (SSH, services)
- Time-based maintenance windows
- CIDR-based access control (APL records)
- Emergency lockdown/recovery modes

### 6. Security Headers

**Module**: `security_headers.ads/adb`

**Features:**
- Server header obfuscation
- Diagnostic mode (authorized IPs only)
- HSTS, CSP, X-Frame-Options
- COEP, COOP, CORP (experimental headers)

### 7. Protocol Security

**Module**: `protocol_manager.ads/adb`

**Features:**
- SNMP v1/v2c **DISABLED** by default (insecure)
- NETCONF/RESTCONF/gNMI recommended over SNMP
- TLS required for all management protocols
- Security validation and warnings

## Known Security Limitations

### Demo Credentials

**Risk**: HIGH
**Status**: DOCUMENTED

The codebase includes demo credentials for development/testing:

```ada
-- src/secure_auth.adb
-- ⚠️  DEMO CREDENTIALS - CHANGE BEFORE DEPLOYMENT
```

**Mitigation**: Clearly documented in README, CLAUDE.md, and inline comments.

**Action Required**: Replace with bcrypt/Argon2 hashed passwords before production.

### No Real Cryptography (Yet)

**Risk**: MEDIUM
**Status**: PLANNED

Currently missing:

- bcrypt/Argon2 password hashing
- TSIG authentication (RFC 2845) - needs crypto library
- SPA packet encryption (needs AES-256-GCM, ChaCha20-Poly1305)

**Mitigation**: Framework is ready, needs crypto library integration.

**Timeline**: Phase 2 (Gold Tier)

### DNS Server Integration Untested

**Risk**: MEDIUM
**Status**: FRAMEWORK COMPLETE

DNS UPDATE (RFC 2136) framework is implemented but not tested with real servers.

**Mitigation**: Extensive logging, firewall rules only logged (not executed) by default.

**Action Required**: Integration testing with BIND/PowerDNS/NSD.

## Security Audit Status

| Audit Type | Status | Last Audit | Next Audit |
|------------|--------|------------|------------|
| **Code Review** | ✅ Ongoing | 2025-11-22 | Continuous |
| **SPARK Verification** | 🔄 Planned | N/A | 2026 Q1 |
| **Penetration Test** | ❌ Not Yet | N/A | After v2.0 |
| **Dependency Audit** | ✅ Clean | 2025-11-22 | Quarterly |

**Dependencies**: ZERO external dependencies (GNAT stdlib only)

## Secure Development Practices

### Build Security

1. **Always-On Safety Checks**
   ```
   -gnato         # Overflow checking
   -fstack-check  # Stack protection
   -gnatVa        # Validity checking
   ```

2. **Security Flags in Release**
   - Assertions enabled even in release builds
   - No unsafe optimizations

### Code Review

- All changes reviewed before merge
- Security-critical modules flagged for extra scrutiny
- SPARK verification planned for auth/SDP modules

### Testing

- Unit tests (planned)
- Integration tests (planned)
- Fuzz testing (planned for DNS packet parsing)

## Security Hall of Fame

We recognize security researchers who responsibly disclose vulnerabilities:

<!-- No reports yet -->

| Date | Researcher | Vulnerability | Severity | Bounty |
|------|------------|---------------|----------|--------|
| TBD  | TBD        | TBD           | TBD      | TBD    |

**Note**: We do not currently offer monetary bounties but provide public credit and gratitude.

## Safe Harbor

We consider security research conducted under this policy to be:

- **Authorized** under applicable anti-hacking laws (CFAA, etc.)
- **Exempt** from DMCA Section 1201 restrictions
- **Lawful, helpful, and appreciated**

We **will not** pursue legal action if you:

- Follow responsible disclosure (report to us privately)
- Allow 90-day window for fix before public disclosure
- Do not access/exfiltrate user data
- Do not harm availability (no DoS attacks on production systems)

## Security Best Practices for Users

If deploying this software:

1. **Change Demo Credentials**
   - Replace all demo passwords
   - Generate unique secrets for production

2. **Enable SDP (Zero-Trust)**
   - Use Software-Defined Perimeter for all access
   - Require Multi-Factor Authentication

3. **Disable Insecure Protocols**
   - No SNMP v1/v2c
   - Use NETCONF/RESTCONF/gNMI

4. **Firewall Configuration**
   - Enable port rotation for SSH
   - Use time-based maintenance windows
   - Restrict by CIDR (APL records)

5. **Logging**
   - Enable audit logging
   - Enable security logging
   - Monitor for anomalies

6. **Keep Updated**
   - Subscribe to security advisories
   - Apply patches promptly
   - Test updates in staging first

## Security Contact

- **Primary**: https://github.com/Hyperpolymath/resource-record-fluctuator/security/advisories/new
- **Email**: security@example.com
- **PGP**: https://github.com/Hyperpolymath.gpg
- **security.txt**: `.well-known/security.txt` (RFC 9116)

## References

- **NIST SP 800-207**: Zero Trust Architecture
- **CSA SDP Specification v2.0**: Software-Defined Perimeter
- **RFC 2136**: DNS Dynamic Updates
- **RFC 2845**: TSIG (Transaction Signatures)
- **RFC 9116**: security.txt
- **Ada 2012**: ISO/IEC 8652:2012
- **SPARK**: High Integrity Software

---

**Last Updated**: 2025-11-22
**Next Review**: 2026-05-22 (6 months)
