# Tri-Perimeter Contribution Framework (TPCF)

**Project**: HINFO-LOC Fluctuator
**Framework Version**: 1.0
**Last Updated**: 2025-11-22

---

## Overview

The **Tri-Perimeter Contribution Framework** (TPCF) is a graduated trust model that balances openness with security. It defines three concentric perimeters of access, each with different responsibilities and privileges.

**Philosophy**: We welcome all contributors while protecting critical infrastructure. Trust is earned through consistent, quality contributions over time.

---

## The Three Perimeters

```
┌────────────────────────────────────────────────────────┐
│ Perimeter 3: Community Sandbox (Everyone)             │
│  ┌──────────────────────────────────────────────────┐ │
│  │ Perimeter 2: Trusted Contributors               │ │
│  │  ┌────────────────────────────────────────────┐ │ │
│  │  │ Perimeter 1: Core Maintainers              │ │ │
│  │  │  - Merge to main                           │ │ │
│  │  │  - Create releases                         │ │ │
│  │  │  - CI/CD configuration                     │ │ │
│  │  └────────────────────────────────────────────┘ │ │
│  │  - Formal review rights                          │ │
│  │  - Issue triage                                  │ │
│  └──────────────────────────────────────────────────┘ │
│  - Fork & PR                                           │
│  - Informal reviews                                    │
│  - Bug reports                                         │
└────────────────────────────────────────────────────────┘
```

---

## Perimeter 3: Community Sandbox

**Who**: Everyone (you!)

**No Approval Needed**: Just start contributing!

### Access Rights

- ✅ Fork the repository
- ✅ Submit pull requests
- ✅ Comment on issues and PRs
- ✅ Review others' code (informal feedback)
- ✅ Report bugs
- ✅ Suggest features
- ✅ Participate in discussions

### Restrictions

- ❌ Cannot merge to main branch
- ❌ Cannot create releases
- ❌ Cannot modify CI/CD configuration
- ❌ Cannot access project credentials

### Responsibilities

- **Quality**: Submit well-tested, documented code
- **Code of Conduct**: Follow community guidelines
- **Security**: Report vulnerabilities responsibly (see SECURITY.md)
- **Communication**: Be respectful and constructive

### Typical Activities

- Fix bugs
- Add features
- Improve documentation
- Write tests
- Help other contributors
- Translate documentation (future)

**Time Commitment**: None! Contribute when you can.

---

## Perimeter 2: Trusted Contributors

**Who**: Regular contributors with proven track record

**How to Join**: Nominated by Perimeter 1 after meeting requirements

### Requirements

1. **5+ merged pull requests** of good quality
2. **Demonstrated understanding** of project goals and architecture
3. **Consistent code quality** (type safety, memory safety, security)
4. **Active participation** in code reviews (informal)
5. **Community positive behavior** (helpful, respectful)
6. **Nomination** by a Perimeter 1 maintainer
7. **Acceptance** by consensus of Perimeter 1

### Access Rights

**Everything in Perimeter 3, plus:**

- ✅ **Formal review rights** (blocking pull requests)
- ✅ **Issue triage** (label, assign, close)
- ✅ **Priority access** for questions and guidance
- ✅ **Recognition** in project documentation
- ✅ **Input on roadmap** and technical direction

### Restrictions

- ❌ Cannot merge to main (Perimeter 1 only)
- ❌ Cannot create releases (Perimeter 1 only)
- ❌ Cannot modify CI/CD (Perimeter 1 only)

### Responsibilities

- **Reviews**: Provide timely, constructive reviews (within 1 week)
- **Triage**: Help manage issue backlog
- **Mentorship**: Guide new contributors (Perimeter 3)
- **Quality**: Maintain high standards
- **Availability**: Responsive to mentions and assignments

### Typical Activities

- Review pull requests (formal, blocking)
- Triage incoming issues
- Label and categorize issues
- Mentor new contributors
- Participate in technical discussions
- Help with release testing

**Time Commitment**: ~2-4 hours/week

### Path to Perimeter 2

**Example Timeline**:

- **Month 1-2**: Submit first PR, get comfortable with codebase
- **Month 3-4**: 3-5 PRs merged, start informal reviews
- **Month 5-6**: 5+ PRs, active in community, nominated for Perimeter 2
- **Month 7+**: Trusted Contributor status granted

**Recognition**: Announced in release notes and listed in MAINTAINERS.md

---

## Perimeter 1: Core Maintainers

**Who**: Project maintainers (see MAINTAINERS.md)

**How to Join**: Invited by existing Perimeter 1 members

### Requirements

1. **6+ months** as Perimeter 2 contributor
2. **Significant contributions** to codebase and community
3. **Deep understanding** of architecture and design decisions
4. **Demonstrated leadership** in reviews, mentoring, direction-setting
5. **Long-term commitment** to project
6. **Invitation** by existing Perimeter 1 members
7. **Unanimous approval** by all Perimeter 1 maintainers

### Access Rights

**Everything in Perimeter 2, plus:**

- ✅ **Merge to main branch**
- ✅ **Create releases** and version tags
- ✅ **Modify CI/CD** configuration
- ✅ **Access credentials** (DNS servers, test infrastructure)
- ✅ **Equal voice** in governance decisions
- ✅ **Emergency powers** (lockdown, rollback)

### Responsibilities

- **Merges**: Review and merge pull requests
- **Releases**: Create and publish releases (semantic versioning)
- **Security**: Respond to vulnerability reports within 48 hours
- **Direction**: Set technical direction and roadmap
- **Governance**: Participate in decision-making
- **Nominations**: Identify and nominate Perimeter 2 candidates
- **Infrastructure**: Maintain CI/CD, testing, deployment
- **Community**: Foster welcoming, inclusive environment

### Typical Activities

- Merge pull requests
- Create releases
- Respond to security reports
- Set roadmap and milestones
- Configure CI/CD
- Manage project credentials
- Make governance decisions
- Nominate/promote contributors

**Time Commitment**: ~5-10 hours/week

### Path to Perimeter 1

**Example Timeline**:

- **Month 1-6**: Perimeter 2 contributor, consistent high-quality work
- **Month 7-12**: Increased responsibilities, technical leadership
- **Month 12-18**: Invitation to Perimeter 1, evaluation period
- **Month 18+**: Full Perimeter 1 maintainer

**Recognition**: Announced publicly, listed prominently in MAINTAINERS.md

---

## Governance and Decision-Making

### Minor Decisions

**Examples**: Code style, bug fixes, documentation improvements

**Process**: Any Perimeter 1 maintainer can approve and merge

**Timeframe**: Immediate to 1 week

### Major Decisions

**Examples**: New features, breaking changes, dependency additions, architecture changes

**Process**: Consensus among Perimeter 1 maintainers

**Timeframe**: 1-2 weeks

**Steps**:
1. Proposal document (issue or RFC)
2. Discussion period (1 week minimum)
3. Perimeter 1 consensus check
4. Implementation (if approved)

### Critical Decisions

**Examples**: License changes, governance changes, Code of Conduct modifications

**Process**: Unanimous approval by Perimeter 1 + community discussion

**Timeframe**: 2-4 weeks

**Steps**:
1. Formal proposal (RFC document)
2. Community discussion period (2 weeks)
3. Perimeter 1 vote (unanimous required)
4. Implementation (if approved)
5. Public announcement

---

## Perimeter Transitions

### Promotion (Perimeter 3 → 2)

**Initiated by**: Perimeter 1 maintainer (nomination)

**Process**:
1. Perimeter 1 member identifies candidate
2. Private discussion among Perimeter 1
3. Consensus reached (simple majority)
4. Invitation sent to candidate
5. Candidate accepts
6. Public announcement
7. Update MAINTAINERS.md

**Timeline**: 1-2 weeks after nomination

### Promotion (Perimeter 2 → 1)

**Initiated by**: Existing Perimeter 1 members

**Process**:
1. Private discussion among all Perimeter 1
2. Unanimous approval required
3. Invitation sent to candidate
4. Candidate accepts
5. Evaluation period (1-3 months with trial access)
6. Final confirmation
7. Public announcement
8. Update MAINTAINERS.md

**Timeline**: 2-4 months after initial discussion

### Demotion or Removal

**Reasons**:
- Inactivity (6+ months, voluntary step-down encouraged)
- Code of Conduct violations
- Security negligence
- Loss of consensus

**Process** (Perimeter 2):
1. Private discussion among Perimeter 1
2. Attempt to resolve issues
3. Simple majority vote
4. Private notification
5. Public announcement (if appropriate)
6. Graceful transition

**Process** (Perimeter 1):
1. Private discussion among remaining Perimeter 1
2. Attempt to resolve issues
3. Unanimous vote (excluding person in question)
4. Private notification
5. Public announcement
6. Graceful transition of responsibilities

---

## Security Considerations

### Perimeter 3 (Untrusted)

**Assumption**: Code is potentially malicious

**Protections**:
- All PRs reviewed before merge
- No direct commit access
- No CI/CD access
- No credential access

### Perimeter 2 (Trusted, Limited)

**Assumption**: Code is well-intentioned but may have bugs

**Protections**:
- Cannot merge own PRs without Perimeter 1 approval
- No CI/CD access
- No credential access
- Review rights are advisory, not final

### Perimeter 1 (Trusted, Full)

**Assumption**: Fully trusted, but still reviewed

**Protections**:
- Consensus required for major changes
- Peer review still encouraged
- Audit logs for all merges
- Emergency rollback capability

---

## Emotional Safety and Well-Being

TPCF is designed to support contributor well-being:

### Perimeter 3: Safe Experimentation

- **Low stakes**: PRs can be rejected without stigma
- **Learning environment**: Mistakes are teaching moments
- **No pressure**: Contribute at your own pace
- **Reversibility**: Easy to fork and try different approaches

### Perimeter 2: Growth and Mentorship

- **Recognition**: Your contributions are valued
- **Voice**: Input on technical direction
- **Mentorship**: Help newcomers, share knowledge
- **Balance**: No obligation if life gets busy

### Perimeter 1: Sustainable Leadership

- **Shared responsibility**: No single point of failure
- **Burnout prevention**: Encourage breaks, step-down if needed
- **Work-life balance**: This is a hobby project, not a job
- **Emeritus status**: Step down gracefully with honor

See also: **Emotional Temperature Metrics** in research documentation

---

## Comparison to Other Models

| Model | TPCF Equivalent | Key Difference |
|-------|-----------------|----------------|
| **Benevolent Dictator** | Perimeter 1 (single) | TPCF supports multi-maintainer consensus |
| **Committer Model** | Perimeter 2 | TPCF separates review rights from merge rights |
| **Meritocracy** | All Perimeters | TPCF explicitly defines merit criteria and transition paths |
| **Do-ocracy** | Perimeter 3 | TPCF adds graduated trust levels |

---

## FAQ

### Can I skip Perimeter 3 if I have experience?

No. Everyone starts in Perimeter 3. This allows us to evaluate fit with our specific project goals, security standards, and community culture.

### How long does it take to reach Perimeter 2?

Typically 3-6 months of consistent contributions. Quality matters more than quantity.

### Can I be in Perimeter 2 of multiple projects?

Yes! TPCF is project-specific. Your status in one project doesn't affect others.

### What if I disagree with a Perimeter 1 decision?

Open discussion is encouraged! Respectfully explain your position. If consensus can't be reached, Perimeter 1 makes the final call, but dissenting opinions are documented.

### Can I lose Perimeter status?

Yes, due to inactivity or violations. However, we strongly prefer voluntary step-down and always attempt to resolve issues before removal.

### Is this just bureaucracy?

No! TPCF clarifies expectations, reduces ambiguity, and makes the path to maintainership transparent. It actually reduces bureaucracy by defining clear processes.

---

## Examples from HINFO-LOC Fluctuator

### Perimeter 3 Success Story

**Scenario**: New contributor fixes typo in documentation

**Process**:
1. Fork repo, fix typo
2. Submit PR
3. Perimeter 1 reviews, suggests minor improvement
4. Contributor updates PR
5. Merged within 24 hours

**Outcome**: Contributor encouraged, documentation improved

### Perimeter 2 Candidate

**Scenario**: Contributor has submitted 6 PRs over 4 months

**Contributions**:
- Added unit tests for DNS_Records module
- Improved error handling in Randomizer
- Wrote comprehensive SPARK contracts for Secure_Auth
- Reviewed 10+ other PRs (informal)
- Helped 3 new contributors

**Nomination**:
- Perimeter 1 nominates to Perimeter 2
- Consensus reached
- Contributor accepts, becomes Trusted Contributor
- Announced in next release notes

### Perimeter 1 Decision

**Scenario**: Should we add DNSSEC support?

**Process**:
1. Community member proposes in issue
2. 2-week discussion period
3. Perimeter 1 evaluates (complexity, maintenance burden, security impact)
4. Consensus: Yes, but as optional feature in v3.0
5. Roadmap updated

---

## Acknowledgments

TPCF is inspired by:
- **Apache Software Foundation** (committer model)
- **Rust Project** (working groups)
- **Python** (core developers, PEPs)
- **CSA SDP** (perimeter-based security)

---

## References

- **MAINTAINERS.md**: Current perimeter assignments
- **CONTRIBUTING.md**: How to contribute
- **CODE_OF_CONDUCT.md**: Community standards
- **SECURITY.md**: Security reporting

---

**Questions?** Open an issue with the `governance` label or contact maintainers.

---

**Next Review**: 2026-05-22 (6 months)
