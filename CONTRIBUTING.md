# Contributing to HINFO-LOC Fluctuator

First off, thank you for considering contributing to HINFO-LOC Fluctuator! It's people like you that make this project a great tool for DNS security and obfuscation.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How Can I Contribute?](#how-can-i-contribute)
- [Development Setup](#development-setup)
- [Pull Request Process](#pull-request-process)
- [Style Guides](#style-guides)
- [TPCF: Tri-Perimeter Contribution Framework](#tpcf-tri-perimeter-contribution-framework)
- [Community](#community)

## Code of Conduct

This project and everyone participating in it is governed by the [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to security@example.com.

## Getting Started

### Prerequisites

To contribute to this project, you'll need:

- **Required**:
  - GNAT Ada compiler (FSF GNAT or AdaCore GNAT Community)
  - Git
  - Basic understanding of DNS concepts

- **Recommended**:
  - Familiarity with Ada 2012
  - Understanding of type safety and memory safety
  - Knowledge of security principles (zero-trust, SDP)

- **Optional**:
  - SPARK Pro (for formal verification)
  - DNS server for testing (BIND, PowerDNS, NSD)

### Your First Contribution

Unsure where to begin? You can start by looking through these issues:

- **good-first-issue** - issues which should only require a few lines of code
- **help-wanted** - issues which need more involvement but are well-defined
- **documentation** - improvements or additions to documentation
- **testing** - help add unit tests, integration tests

## How Can I Contribute?

### Reporting Bugs

**Security Vulnerabilities**: See [SECURITY.md](SECURITY.md) - DO NOT open public issues.

**Non-Security Bugs**:

1. **Check existing issues** - it may already be reported
2. **Create a new issue** with:
   - Clear title and description
   - Steps to reproduce
   - Expected vs. actual behavior
   - Ada compiler version, OS, system details
   - Code sample (if applicable)

### Suggesting Enhancements

We welcome feature requests! Before suggesting:

1. **Check the roadmap** - see CLAUDE.md for planned features
2. **Search existing issues** - it may have been suggested
3. **Create a detailed proposal** including:
   - Use case (why is this needed?)
   - Proposed solution
   - Alternatives considered
   - Impact on type safety, memory safety, security

### Code Contributions

1. **Fork the repository**
2. **Create a branch** from `main`:
   ```bash
   git checkout -b feature/my-new-feature
   ```
3. **Make your changes** following our [Style Guides](#style-guides)
4. **Test your changes**:
   ```bash
   cd hinfo_loc_fluctuator_ada
   make          # Debug build
   make release  # Release build
   make prove    # SPARK verification (if applicable)
   ```
5. **Commit your changes** with clear messages (see [Commit Messages](#commit-messages))
6. **Push to your fork**
7. **Submit a Pull Request**

## Development Setup

### Clone and Build

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/resource-record-fluctuator.git
cd resource-record-fluctuator/hinfo_loc_fluctuator_ada

# Build debug version
make

# Run the application
./bin/hinfo_loc_fluctuator

# Demo credentials (DO NOT use in production):
# Username: admin
# Password: (see README.md)
```

### Project Structure

```
hinfo_loc_fluctuator_ada/
├── src/              # Ada source files (.ads/.adb)
├── data/             # CPU, OS, location data
├── docs/             # Documentation
├── examples/         # Example configurations
├── obj/              # Build artifacts (git-ignored)
└── bin/              # Compiled binaries (git-ignored)
```

### Key Modules

- **dns_records.ads/adb**: Core DNS record types (HINFO, LOC)
- **dns_records_extended.ads/adb**: All 20+ DNS record types
- **secure_auth.ads/adb**: Authentication and authorization
- **randomizer.ads/adb**: Randomization engine
- **sdp_controller.ads/adb**: Zero-Trust SDP
- **firewall_manager.ads/adb**: Firewall integration
- **security_headers.ads/adb**: HTTP security headers
- **protocol_manager.ads/adb**: NETCONF/RESTCONF/gNMI/SNMP

## Pull Request Process

1. **Update documentation** if you're adding features
2. **Add tests** for new functionality (when test framework exists)
3. **Ensure security** - no new vulnerabilities introduced
4. **Run the build** - must compile without errors/warnings
5. **SPARK verification** - if modifying security-critical modules
6. **Update CHANGELOG.md** - describe your changes
7. **Self-review** - check your own code before submitting
8. **Request review** - from maintainers
9. **Address feedback** - be responsive to review comments
10. **Squash commits** - if requested
11. **Wait for approval** - at least one maintainer approval required

### PR Checklist

- [ ] Code compiles without warnings
- [ ] Security flags pass (`-gnato`, `-fstack-check`, `-gnatVa`)
- [ ] Documentation updated (if applicable)
- [ ] CHANGELOG.md updated
- [ ] No demo credentials committed
- [ ] CLAUDE.md updated (if architecture changes)
- [ ] Commit messages follow convention
- [ ] Tests pass (when test framework exists)

## Style Guides

### Ada Code Style

Follow standard Ada conventions:

```ada
--  Package Specification
package My_Package is

   --  Type names: Mixed_Case_With_Underscores
   type My_Type is range 1 .. 100;

   --  Constant names: UPPER_CASE_WITH_UNDERSCORES
   MAX_RETRIES : constant := 3;

   --  Procedure/Function names: Mixed_Case
   procedure Do_Something (Param : My_Type);

end My_Package;
```

**Guidelines:**
- **Indentation**: 3 spaces (Ada standard)
- **Line length**: 100 characters max
- **Comments**: Explain "why", not "what"
- **Type safety**: Use strong typing, avoid conversions
- **No magic numbers**: Use named constants
- **Error handling**: Use exceptions for exceptional cases
- **Documentation**: Add comments for public interfaces

### Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>: <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `refactor`: Code refactoring
- `test`: Adding tests
- `chore`: Build process, dependencies

**Example:**
```
feat: Add port rotation for SSH service

Implements time-based port rotation with configurable intervals.
Maintainers can calculate the current port offline using the
algorithm: port = base + (timestamp / interval) % range

Closes #42
```

### Documentation Style

- **Markdown**: Use GitHub-flavored markdown
- **Code blocks**: Specify language (```ada, ```bash)
- **Links**: Use reference-style for readability
- **Headers**: Use ATX style (# ## ###)
- **Lists**: Use - for unordered, 1. 2. 3. for ordered

## TPCF: Tri-Perimeter Contribution Framework

This project uses the **Tri-Perimeter Contribution Framework** (TPCF) with graduated trust levels.

### Perimeter 3: Community Sandbox (Current)

**Who**: Everyone (you!)

**Access**:
- Fork repository
- Submit pull requests
- Comment on issues
- Review others' PRs (informal)

**Restrictions**:
- Cannot merge to main
- Cannot create releases
- Cannot modify CI/CD

**How to Join**: Just start contributing! No approval needed.

### Perimeter 2: Trusted Contributors (Future)

**Who**: Regular contributors with proven track record

**Access**:
- Everything in Perimeter 3, plus:
- Review PRs (formal, blocking)
- Triage issues
- Label and assign issues

**How to Join**: Nominated by Perimeter 1 after 5+ quality contributions

### Perimeter 1: Core Maintainers

**Who**: Project maintainers (see MAINTAINERS.md)

**Access**:
- Everything in Perimeter 2, plus:
- Merge to main
- Create releases
- Modify CI/CD
- Access credentials (for testing)

**How to Join**: Invited by existing Perimeter 1 members

For more details, see [TPCF.md](TPCF.md).

## Community

### Communication Channels

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: General questions, ideas (when enabled)
- **Pull Requests**: Code review, technical discussions

### Getting Help

- **Documentation**: Start with [README.md](hinfo_loc_fluctuator_ada/README.md)
- **Use Cases**: See [USE_CASES.md](hinfo_loc_fluctuator_ada/docs/USE_CASES.md)
- **Enterprise Features**: See [ENTERPRISE_FEATURES.md](hinfo_loc_fluctuator_ada/docs/ENTERPRISE_FEATURES.md)
- **Project Context**: See [CLAUDE.md](CLAUDE.md)
- **Open an Issue**: Ask questions, we're friendly!

### Ada Resources

New to Ada? Check these out:

- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)
- [GNAT User's Guide](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn.html)
- [Learn Ada](https://learn.adacore.com/)

## Recognition

Contributors will be recognized in:

- **CHANGELOG.md**: For each release
- **humans.txt**: In `.well-known/humans.txt`
- **Git history**: Your commits are your legacy!

We especially recognize:

- **Security researchers**: See SECURITY.md Hall of Fame
- **First-time contributors**: Your first PR is celebrated!
- **Documentation improvers**: Docs are as important as code!

## License

By contributing, you agree that your contributions will be licensed under the same license as the project:

- **MIT License** (default)
- **Palimpsest v0.8** (alternative, when added)

See [LICENSE](LICENSE) for details.

---

## Questions?

Don't hesitate to ask! Open an issue with the `question` label, or reach out to maintainers.

**Welcome aboard, and happy contributing!** 🚀

---

**Last Updated**: 2025-11-22
