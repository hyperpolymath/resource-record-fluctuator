# Maintainers

This document lists the current maintainers of the HINFO-LOC Fluctuator project and describes the governance structure.

## Current Maintainers

### Perimeter 1: Core Maintainers

| Name | GitHub | Role | Areas of Focus | Since |
|------|--------|------|----------------|-------|
| Hyperpolymath | [@Hyperpolymath](https://github.com/Hyperpolymath) | Project Lead, Architecture | Overall project direction, Ada implementation, enterprise features | 2025-11 |

## Governance Model

This project uses the **Tri-Perimeter Contribution Framework** (TPCF) with three levels of access:

### Perimeter 1: Core Maintainers (Above)
- **Responsibilities**:
  - Merge pull requests to main
  - Create releases
  - Set project direction
  - Manage CI/CD and credentials
  - Nominate Perimeter 2 members

- **Decision Making**: Consensus-based (currently single maintainer)

### Perimeter 2: Trusted Contributors (None Currently)
- **Responsibilities**:
  - Review pull requests (blocking)
  - Triage issues
  - Label and assign issues
  - Mentor new contributors

- **How to Join**: Nominated by Perimeter 1 after 5+ quality contributions

### Perimeter 3: Community Sandbox (Everyone)
- **Responsibilities**:
  - Submit pull requests
  - Review others' work (informal)
  - Report bugs and suggest features
  - Help other contributors

- **How to Join**: Just start contributing!

See [TPCF.md](TPCF.md) for detailed framework documentation.

## Areas of Expertise

We're looking for contributors with expertise in:

### Core Development
- **Ada/SPARK**: Type-safe systems programming, formal verification
- **DNS Protocols**: RFC implementation, BIND/PowerDNS/NSD
- **Security Architecture**: Zero-trust, SDP, cryptography

### Enterprise Features
- **Firewall Management**: firewalld, iptables, nftables, pf
- **Network Protocols**: NETCONF, RESTCONF, gNMI, SNMP
- **Security Headers**: HTTP security, obfuscation techniques

### Operations
- **Testing**: Unit tests, integration tests, fuzz testing
- **CI/CD**: GitLab CI, GitHub Actions
- **Packaging**: Nix, system packages (apt, yum, pacman)

### Documentation
- **Technical Writing**: Clear, comprehensive documentation
- **Use Cases**: Security research, honeypot deployment
- **Translation**: Non-English documentation (future)

## Becoming a Maintainer

### Path to Perimeter 2 (Trusted Contributor)

**Requirements**:
1. 5+ merged pull requests
2. Demonstrated understanding of project goals
3. Consistent code quality
4. Active participation in reviews
5. Nomination by Perimeter 1 member
6. Acceptance by consensus

**Benefits**:
- Formal review rights (blocking PRs)
- Issue triage and labeling
- Recognition in project documentation
- Input on project direction

### Path to Perimeter 1 (Core Maintainer)

**Requirements**:
1. 6+ months as Perimeter 2 contributor
2. Significant contributions to codebase
3. Deep understanding of architecture
4. Demonstrated leadership
5. Invitation by existing Perimeter 1 members
6. Unanimous approval by Perimeter 1

**Benefits**:
- Merge rights to main branch
- Release creation
- CI/CD configuration
- Access to project credentials
- Equal voice in governance decisions

## Responsibilities

### All Maintainers

- **Code Quality**: Maintain high standards for type safety, memory safety, security
- **Reviews**: Provide timely, constructive code reviews
- **Communication**: Respond to issues and PRs within 1 week
- **Documentation**: Keep docs up-to-date
- **Community**: Foster welcoming, inclusive environment
- **Security**: Follow responsible disclosure, prioritize security

### Perimeter 1 Specific

- **Releases**: Create and publish releases (semantic versioning)
- **Security**: Respond to vulnerability reports within 48 hours
- **Direction**: Set technical direction and roadmap
- **Nominations**: Identify and nominate Perimeter 2 candidates
- **CI/CD**: Maintain build pipeline and testing infrastructure

## Decision-Making Process

### Minor Decisions
- Code style, formatting
- Bug fixes
- Documentation improvements
- **Process**: Any maintainer can approve and merge

### Major Decisions
- New features
- Breaking changes
- Dependency additions
- Architecture changes
- **Process**: Consensus among Perimeter 1 (currently single maintainer)

### Critical Decisions
- License changes
- Governance changes
- Code of Conduct modifications
- **Process**: Unanimous approval by Perimeter 1 + community discussion period (2 weeks)

## Removing Maintainers

Maintainers may be removed due to:

- **Inactivity**: No participation for 6+ months (voluntary step-down encouraged)
- **Code of Conduct Violations**: Serious or repeated violations
- **Security Negligence**: Intentional or reckless security issues
- **Lack of Consensus**: Persistent disagreement with project direction

**Process**:
1. Private discussion among Perimeter 1
2. Attempt to resolve issues
3. Vote (unanimous for Perimeter 1 removal)
4. Public announcement (if appropriate)
5. Graceful transition of responsibilities

## Emeritus Maintainers

Former maintainers who stepped down in good standing:

| Name | GitHub | Tenure | Contributions |
|------|--------|--------|---------------|
| (None yet) | - | - | - |

## Contact

- **General Questions**: Open a GitHub issue
- **Private Concerns**: security@example.com
- **Governance Questions**: maintainers@example.com (future)

## Acknowledgments

We're grateful to all contributors, regardless of perimeter level. Every contribution matters!

---

**Last Updated**: 2025-11-22
**Next Review**: 2026-05-22 (6 months)
