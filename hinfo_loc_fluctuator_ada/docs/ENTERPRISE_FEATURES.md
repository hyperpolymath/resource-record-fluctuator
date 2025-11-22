# Enterprise Features Guide

## Overview

The HINFO-LOC Fluctuator has been expanded into a comprehensive **DNS Security and Infrastructure Management Platform** with enterprise-grade features for zero-trust access, dynamic firewall control, and advanced obfuscation.

## Table of Contents

1. [Extended DNS Records](#extended-dns-records)
2. [Firewall Integration](#firewall-integration)
3. [Port Rotation](#port-rotation)
4. [Security Headers](#security-headers)
5. [Software-Defined Perimeter](#software-defined-perimeter)
6. [Protocol Management](#protocol-management)
7. [Service Scheduling](#service-scheduling)
8. [Configuration](#configuration)

---

## Extended DNS Records

### Supported Record Types

**Basic Address Records:**
- `A` (IPv4) and `AAAA` (IPv6) with protocol toggle
- `PTR` (Reverse DNS)

**Delegation and Aliasing:**
- `NS` (Name Server)
- `CNAME` (Canonical Name)

**Mail Records:**
- `MX` (Mail Exchange) with time-based scheduling
- `SPF` (Sender Policy Framework)
- `DKIM` (DomainKeys Identified Mail)
- `DMARC` (Domain-based Message Authentication)

**Service Records:**
- `SRV` (Service Location) with enable/disable toggle
- `NAPTR` (Naming Authority Pointer)

**Security Records:**
- `CAA` (Certification Authority Authorization)
- `TLSA` (DANE - DNS-based Authentication)
- `SSHFP` (SSH Fingerprint)
- `APL` (Address Prefix List) for CIDR-based access control

**DNSSEC:**
- `DS` (Delegation Signer)
- `DNSKEY` (DNS Public Key)
- `NSEC` / `NSEC3` (Next Secure)

### IPv4/IPv6 Protocol Toggle

```ada
Set_IP_Protocol_Mode (Dual_Stack);    --  Both IPv4 and IPv6
Set_IP_Protocol_Mode (IPv4_Only);     --  IPv4 only
Set_IP_Protocol_Mode (IPv6_Only);     --  IPv6 only
Set_IP_Protocol_Mode (Disabled);      --  Neither (diagnostic)
```

### DNS Topology Modes

Supported configurations:
- **Standard**: Single primary, multiple secondaries
- **Split-Horizon**: Different views for internal/external
- **Primary-Primary**: Multi-master replication
- **Hidden Primary**: Primary not publicly visible
- **Secondary-Only**: Read-only secondary
- **Stealth Secondary**: Secondary not listed in NS records

### AXFR Configuration

```ini
[dns_topology]
mode=hidden_primary
axfr_enabled=true
axfr_tsig_required=true
allowed_transfer_ips=192.168.1.10/32,10.0.0.5/32
```

---

## Firewall Integration

### Supported Firewall Backends

- **firewalld** (Linux, recommended)
- **iptables** (Linux, direct)
- **nftables** (Linux, modern)
- **pf** (BSD)

### Time-Based Access Control

Define maintenance windows with IP restrictions:

```ada
type Maintenance_Window is record
   Window      : Time_Window;  --  Start/End times
   Description : "Weekly maintenance";
   Allowed_IPs : [192.168.1.0/24, 10.0.0.0/8];
   Ports       : [22, 443, 8443];
end record;
```

### Stateful vs Stateless Rules

- **Stateful**: Connection tracking (default)
- **Stateless**: No tracking (faster, less secure)

### Firewall Zones (firewalld)

- Drop, Block, Public, External, DMZ, Work, Home, Internal, Trusted

---

## Port Rotation

### SSH Port Rotation Strategies

**1. Time-Based** (maintainers can calculate independently):
```ini
[port_rotation]
service=ssh
strategy=time_based
base_port=10000
port_range=1000
rotation_interval=3600  # 1 hour
```

**Algorithm** (shared with maintainers):
```
current_port = base_port + (unix_timestamp / rotation_interval) % port_range
```

**Example**: If base=10000, range=1000, interval=3600:
- At 00:00 UTC: Port 10000 + (timestamp/3600) % 1000
- Maintainers can calculate this offline

**2. Pre-Shared Key** (algorithm with secret):
```
current_port = base_port + hash(PSK + datetime) % port_range
```

**3. Sequential**: 10000, 10001, 10002, ...

**4. Random**: Pseudorandom from pool

### Service Port Rotation

Apply to any service:
- SSH
- Admin panels
- Management interfaces
- Custom services

---

## Security Headers

### Server Obfuscation Modes

**1. Hidden**: Don't send header at all
```ini
[security_headers]
server_header_mode=hidden
```

**2. Obfuscated**: Send fake value
```ini
server_header_mode=obfuscated
server_fake_value=Apache/2.4.41 (Unix)
randomize_fake=true
```

**3. Diagnostic**: Show real to authorized IPs only
```ini
server_header_mode=diagnostic
server_real_value=nginx/1.21.0
diagnostic_ips=192.168.1.100/32,10.0.0.5/32
```

**4. Standard**: Show real to everyone (not recommended)

### X-Powered-By Obfuscation

```ini
[security_headers]
powered_by_mode=hidden  # Recommended
# OR
powered_by_mode=obfuscated
powered_by_fake=PHP/7.4.3
```

### Diagnostic Mode

Access real stack info with secret token:
```
GET /diagnostics?token=<secret-token>
X-Diagnostic-Token: <secret-token>
```

Response (JSON):
```json
{
  "server": "nginx/1.21.0",
  "php_version": "8.1.2",
  "framework": "Laravel 9.0",
  "database": "PostgreSQL 14.1",
  "modules": [...],
  "security_headers": {...]
}
```

### Standard Security Headers

All modern security headers supported:
- HSTS (Strict-Transport-Security)
- CSP (Content-Security-Policy) with nonces
- X-Frame-Options
- X-Content-Type-Options
- Referrer-Policy
- Permissions-Policy

### Experimental Headers

- COEP (Cross-Origin-Embedder-Policy)
- COOP (Cross-Origin-Opener-Policy)
- CORP (Cross-Origin-Resource-Policy)
- Expect-CT (being deprecated)
- NEL (Network Error Logging)

### Integration with HINFO Records

Keep server headers consistent with HINFO DNS records:

```ada
Sync_With_HINFO(
   Config     => Security_Header_Set,
   HINFO_CPU  => "Intel-Xeon",
   HINFO_OS   => "Ubuntu-22.04"
);
```

Now both DNS and HTTP headers show consistent (fake) stack.

---

## Software-Defined Perimeter (SDP)

### Zero-Trust Architecture

Implements Cloud Security Alliance SDP specification:

1. **Default Deny**: All ports closed by default
2. **Single Packet Authorization (SPA)**: Encrypted knock sequence
3. **Identity-Based Access**: User + device authentication
4. **Continuous Verification**: Periodic re-authentication
5. **Micro-Segmentation**: Network isolation

### Single Packet Authorization (SPA)

Encrypted authentication in single UDP packet:

```
[SPA Packet] → SDP Controller → Validates → Opens Firewall
```

**Encryption**: AES-256-GCM, ChaCha20-Poly1305
**HMAC**: SHA256, SHA512, SHA3-256

### Device Posture Validation

Requirements before access:
- OS version minimum
- Antivirus running
- Firewall enabled
- Disk encryption
- Patch level current
- Custom checks

### Trust Levels

Progressive trust with continuous verification:
1. Untrusted (default)
2. Device Verified (certificate valid)
3. User Authenticated (MFA passed)
4. Posture Valid (security checks passed)
5. Full Trust (all requirements met)

### Access Policies

```ada
type Access_Policy is record
   Policy_Name       : "Admin SSH Access";
   Required_Trust    : Full_Trust;
   Allowed_Users     : "alice,bob";
   Allowed_Groups    : "admins";
   Time_Restrictions : Weekdays_Business_Hours;
   Source_CIDR       : [192.168.1.0/24];
   Destination       : "server.example.com";
   Ports             : [22];
   Protocol          : "tcp";
   Session_Duration  : 3600;  -- 1 hour
   Continuous_Verify : true;   -- Re-check every 5 min
end record;
```

### Micro-Segmentation

Network segments with isolation:

```
[Public DMZ] ←X→ [Application Tier] ←X→ [Database Tier]
      ↓ (SDP only)        ↓ (SDP only)        ↓ (SDP only)
   Allowed via policy enforcement
```

### Session Management

Active sessions with automatic expiry and firewall cleanup.

---

## Protocol Management

### Recommended: NETCONF/RESTCONF/gNMI

**NOT SNMP** (insecure, deprecated for new deployments)

### SNMP Configuration (if required)

**⚠️ WARNINGS:**
- SNMPv1/v2c send community strings in plaintext
- ONLY use SNMPv3 with encryption
- Better: Use NETCONF, RESTCONF, or gNMI instead

```ini
[snmp]
enabled=false  # Disabled by default
warn_insecure=true
require_v3_only=true

[snmp_v3]
enabled=false
security_name=admin
auth_protocol=SHA-256
auth_password=<strong-password>
priv_protocol=AES256
priv_password=<strong-password>
allowed_ips=192.168.1.100/32
read_only=true
```

### NETCONF (Recommended)

XML-based configuration over SSH:

```ini
[netconf]
enabled=true
port=830
ssh_host_key=/etc/ssh/ssh_host_rsa_key
capabilities=base-1.1,candidate,confirmed-commit,validate
auth_method=publickey
require_mfa=true
allowed_ips=192.168.1.0/24
max_sessions=10
session_timeout=600
```

### RESTCONF (Recommended)

RESTful API over HTTPS:

```ini
[restconf]
enabled=true
port=443
base_path=/restconf
auth_method=client_certificate
tls_version=1.3
certificate=/etc/ssl/server.crt
private_key=/etc/ssl/server.key
client_ca=/etc/ssl/client-ca.crt
rate_limit=100  # requests per minute
accept_json=true
```

### gNMI (Modern Alternative)

gRPC-based management:

```ini
[gnmi]
enabled=true
port=9339
tls_enabled=true
certificate=/etc/ssl/gnmi.crt
private_key=/etc/ssl/gnmi.key
encoding=JSON_IETF
subscribe=true  # Streaming telemetry
get_enabled=true
set_enabled=false  # Read-only
```

### Prometheus/OpenMetrics

Metrics export:

```ini
[metrics]
enabled=true
protocol=prometheus
port=9090
metrics_path=/metrics
auth_required=true
auth_token=<bearer-token>
client_certs=true
rate_limit=60
```

---

## Service Scheduling

### Time-Based Service Availability

Open services only during specific windows:

### MX (Mail) Scheduling

Accept mail only during business hours:

```ini
[service_schedule_mx]
enabled=true
service=smtp
port=25
protocol=tcp
windows=weekdays:08:00-18:00,saturday:09:00-13:00
randomized=false
```

### RSS Feed Scheduling

Make RSS available only at intervals:

```ini
[service_schedule_rss]
enabled=true
service=http
port=80
path=/rss
windows=hourly:00-05  # First 5 minutes of every hour
randomized=true  # Add random offset
```

### NNTP Scheduling

News server with randomized access:

```ini
[service_schedule_nntp]
enabled=true
service=nntp
port=119
protocol=tcp
randomized=true
random_window_duration=300  # 5 minutes
random_interval=3600  # Every hour, random 5-min window
```

### Why Schedule Services?

**Security Benefits:**
1. Reduced attack surface (service not always available)
2. Makes scanning harder (port appears closed most of time)
3. Deception value (appears misconfigured to attackers)

**Use Cases:**
- Honeypot obfuscation
- Maintenance windows
- Rate limiting
- Resource management

---

## Configuration

### Configuration Hierarchy

1. **Simple Config** (`config.ini`) - Basic settings
2. **Master Config** (`master_config.yaml`) - Enterprise features
3. **Runtime Overrides** - API or command-line

### Deployment Profiles

**Development**:
- All features enabled
- Verbose logging
- Diagnostics available
- Insecure protocols allowed (for testing)

**Staging**:
- Production-like
- Testing features enabled
- Moderate logging

**Production**:
- Maximum security
- Minimal exposure
- Audit logging
- Insecure protocols disabled

**Honeypot**:
- Deception-focused
- Fluctuation enabled
- Service scheduling active
- Extensive logging

**Research**:
- Experimental features
- All protocols enabled
- Detailed metrics

### Example Master Config

```yaml
deployment:
  mode: production
  environment: public_internet
  instance_name: prod-dns-01

dns:
  topology: hidden_primary
  ip_protocol_mode: dual_stack
  dnssec_enabled: false

fluctuation:
  hinfo_enabled: true
  loc_enabled: true
  quantum_server_mode: true

firewall:
  backend: firewalld
  port_rotation_enabled: true
  ssh_port_rotation: true

security_headers:
  server_header_mode: obfuscated
  powered_by_mode: hidden
  diagnostic_mode_enabled: true
  hsts_enabled: true
  csp_enabled: true

sdp:
  enabled: true
  spa_encryption: AES_256_GCM
  default_deny: true
  continuous_verify: true

protocols:
  snmp_allowed: false
  netconf_preferred: true
  restconf_enabled: true
  gnmi_enabled: true

service_scheduling:
  mx_scheduled: false
  ssh_port_rotation: true

logging:
  audit_logging: true
  security_logging: true
  syslog_server: syslog.example.com
```

### Validation

Configuration is validated on load:

```
✓ Security posture: Strong
✗ Warning: SNMPv2c enabled (insecure)
✓ Firewall: Configured correctly
✓ SDP: Zero-trust enabled
! Note: Consider enabling DNSSEC
```

---

## Integration Examples

### Example 1: Public Web Server with SDP

```
1. Client requests access via SPA packet
2. SDP validates: user + device + posture
3. Firewall opens port 443 for client IP only
4. HTTPS connection established
5. Security headers sent (obfuscated)
6. After session timeout, firewall closes
```

### Example 2: Honeypot with Full Deception

```
DNS Records:
- HINFO: Changes every hour (random CPU/OS)
- LOC: Changes every 30 min (random location)
- MX: Only accepts mail 9am-5pm weekdays
- SSH: Port rotates every 2 hours (10000-10999)

Security Headers:
- Server: Randomized (Apache, nginx, IIS, etc.)
- X-Powered-By: Hidden

Result: Attacker sees inconsistent, unstable target
```

### Example 3: Secure Admin Access

```
SSH Access:
- Port rotates every hour (time-based algorithm)
- SDP required (user + device authentication)
- Firewall only opens during maintenance windows
- Source IP must be in approved CIDR
- Continuous posture verification

Maintainers:
- Calculate current SSH port offline
- Send SPA packet
- Port opens for 1 hour
- SSH as normal
```

---

## Best Practices

### Security

1. **Always use SDP** for sensitive services
2. **Disable SNMPv1/v2c** - use NETCONF or gNMI
3. **Enable DNSSEC** for production
4. **Use time-based port rotation** for admin access
5. **Obfuscate all server headers**
6. **Enable audit logging**

### Performance

1. **Use stateful firewall** for normal traffic
2. **Use stateless firewall** for high-performance scenarios
3. **Limit service scheduling** to non-critical services
4. **Cache DNS records** appropriately

### Operational

1. **Test in staging** before production
2. **Document maintenance windows**
3. **Keep emergency recovery config** handy
4. **Monitor SDP session logs**
5. **Regular security audits**

---

## Troubleshooting

### Port Rotation

**Problem**: Can't SSH after port rotation

**Solution**: Calculate current port:
```python
import time
base_port = 10000
rotation_interval = 3600  # 1 hour
port_range = 1000

current_port = base_port + (int(time.time()) // rotation_interval) % port_range
print(f"Current SSH port: {current_port}")
```

### SDP Access

**Problem**: SPA packet not opening firewall

**Check**:
1. Correct shared key?
2. Clock synchronized (NTP)?
3. Client IP in allowed CIDR?
4. Device posture valid?
5. User authenticated?

### Service Scheduling

**Problem**: Service not available during window

**Check**:
1. Correct timezone configured?
2. Firewall rules applied?
3. Service actually running?
4. Check logs for schedule evaluation

---

## Security Considerations

### Threats Mitigated

✅ Port scanning (ports rotate/close)
✅ Service fingerprinting (headers obfuscated)
✅ Geographic tracking (LOC fluctuates)
✅ Stack fingerprinting (HINFO fluctuates)
✅ Unauthorized access (SDP + firewall)
✅ Timing attacks (constant-time operations)
✅ Replay attacks (SPA nonces)

### Threats NOT Mitigated

❌ DDoS attacks (need separate DDoS protection)
❌ Application-level vulnerabilities
❌ Social engineering
❌ Insider threats (partial mitigation via audit logs)
❌ Zero-day exploits

### Compliance

Features support:
- NIST Zero Trust Architecture (SP 800-207)
- CSA Software-Defined Perimeter
- PCI DSS (firewall + access control requirements)
- HIPAA (access controls + audit logging)
- SOC 2 (security controls + monitoring)

---

## Migration Guide

### From Simple Config

1. Install enterprise modules
2. Run migration tool: `migrate_config simple_config.ini master_config.yaml`
3. Review generated configuration
4. Test in staging
5. Deploy to production

### Incremental Adoption

Don't enable everything at once:

**Phase 1**: Extended DNS records
**Phase 2**: Firewall integration
**Phase 3**: Security headers
**Phase 4**: Port rotation
**Phase 5**: Service scheduling
**Phase 6**: SDP (zero-trust)
**Phase 7**: Protocol management

---

## Support and Resources

- **Documentation**: See `docs/` directory
- **Examples**: See `examples/` directory
- **Issue Tracker**: [GitHub Issues]
- **Security**: security@example.com

---

**Remember**: With great power comes great responsibility. Test thoroughly before production deployment!
