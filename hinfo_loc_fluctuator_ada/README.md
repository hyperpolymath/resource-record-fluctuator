# HINFO-LOC Fluctuator - Quantum Server™

DNS record randomization tool for deprecated HINFO (host info) and LOC (geographic location) records. Built with Ada for maximum security and type safety.

## 🎯 Purpose

**Amusing Applications:**
- "Quantum Server" - make your server appear to exist in multiple locations simultaneously
- Confuse network reconnaissance tools
- Entertainment and demonstration

**Serious Security Uses:**
- Honeypot obfuscation (make honeypots appear to move locations/change hardware)
- Attack response and deception technology
- Privacy enhancement for public-facing DNS records
- Security research and education

**Important:** HINFO and LOC records are deprecated per RFC 8482 - that's precisely the point! These records are rarely used in production, making them ideal for experimental security techniques without affecting critical services.

## ✨ Features

### Implemented ✅

- **Type-Safe DNS Records**
  - HINFO records (CPU + Operating System)
  - LOC records (Geographic location with altitude)
  - Compile-time bounds checking prevents invalid values
  - TTL range: 1-604800 seconds (1 second to 7 days)
  - Latitude: -90° to +90° with 6 decimal precision
  - Longitude: -180° to +180° with 6 decimal precision
  - Altitude: -100,000m to 42,849,672m (Dead Sea to space)

- **Security-First Design**
  - Permission system: None → Read-Only → Modify-Local → Modify-Remote → Admin
  - Session management with automatic 30-minute timeout
  - Constant-time password comparison (prevents timing attacks)
  - Replay attack detection using nonces
  - All compiler safety checks enabled even in release builds
  - No buffer overflows possible (Ada memory safety)

- **Randomization Engine**
  - Load CPU types from `data/machines.txt` (68 included)
  - Load OS names from `data/operating_systems.txt` (77 included)
  - Load locations from `data/locations.csv` (52 included)
  - Full CSV parser implementation
  - Generate random HINFO and LOC records
  - "Quantum Server" mode (generates both simultaneously)

- **Interactive TUI**
  - Menu-driven interface with ANSI colors
  - Login screen with permission-based access
  - Pool management and status display
  - Record generation and preview

- **Build System**
  - GNAT project file with multiple build modes
  - Makefile with convenient targets
  - Debug, Release, and SPARK verification modes
  - Overflow checking ALWAYS enabled

### Not Yet Implemented ❌

- DNS UPDATE (RFC 2136) for remote server modification
- TSIG authentication (RFC 2845) for secure updates
- Zone file writer for local mode (BIND format)
- Scheduled auto-fluctuation with Ada tasking
- Configuration file support (TOML/YAML)
- API/webhook triggers
- Production-grade crypto (bcrypt/Argon2)
- SPARK formal verification proofs

## 🚀 Quick Start

### Prerequisites

- GNAT Ada compiler (FSF GNAT or AdaCore GNAT Community Edition)
- Make (for build automation)
- Linux/Unix environment (or WSL on Windows)

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd resource-record-fluctuator/hinfo_loc_fluctuator_ada

# Build the project
make

# Run the program
./bin/hinfo_loc_fluctuator
```

### Demo Credentials

**⚠️  WARNING: These are DEMO credentials with simplified crypto!**

| Username | Password | Permission Level |
|----------|----------|------------------|
| admin    | any      | Full access (Admin) |
| user     | any      | Read-only access |
| operator | any      | Local modifications |

**MUST be replaced before any real deployment!**

## 📖 Usage

### Interactive Mode (Default)

```bash
./bin/hinfo_loc_fluctuator
```

1. Login with demo credentials
2. Load data pools (Option 1)
3. Generate random records (Option 3)
4. Try "Quantum Server" mode (Option 4)

### Command-Line Options

```bash
./bin/hinfo_loc_fluctuator --help      # Show help
./bin/hinfo_loc_fluctuator --version   # Show version
./bin/hinfo_loc_fluctuator --interactive   # Explicit interactive mode
```

### Building Different Modes

```bash
make debug          # Debug build (default)
make release        # Optimized build (safety checks still enabled!)
make prove          # SPARK verification mode
make verify         # Run SPARK formal verification
make clean          # Clean build artifacts
make install        # Install to /usr/local/bin (requires sudo)
```

## 📂 Project Structure

```
hinfo_loc_fluctuator_ada/
├── src/
│   ├── dns_records.ads/adb    # Type-safe DNS record definitions
│   ├── secure_auth.ads/adb    # Authentication and session management
│   ├── randomizer.ads/adb     # Data loading and random generation
│   ├── tui.ads/adb            # Text user interface
│   └── main.adb               # Entry point
├── data/
│   ├── machines.txt           # 68 CPU types
│   ├── operating_systems.txt  # 77 operating systems
│   └── locations.csv          # 52 geographic locations
├── hinfo_loc_fluctuator.gpr   # GNAT project file
├── Makefile                   # Build automation
├── README.md                  # This file
└── docs/
    └── USE_CASES.md           # Detailed use case scenarios
```

## 🔧 Configuration

### Adding Custom CPUs/Operating Systems

Edit `data/machines.txt` or `data/operating_systems.txt`:

```
# One entry per line
# Lines starting with # are comments

Intel-Core-i9
AMD-Ryzen-9
Apple-M2
# Add your custom entries here
```

### Adding Custom Locations

Edit `data/locations.csv`:

```csv
# Format: latitude,longitude,altitude_meters,description
37.7749,-122.4194,16,San Francisco Data Center
51.5074,-0.1278,11,London Data Center
# Add your custom locations here
```

## 🛡️ Security Considerations

### Compile-Time Safety

Ada's type system prevents entire classes of bugs:

```ada
type Latitude_Degrees is delta 0.000001 range -90.0 .. 90.0;
-- Compiler GUARANTEES latitude is valid - no runtime check needed
```

### Enabled Security Flags

Even in release builds:
- `-gnato`: Overflow checking on all arithmetic
- `-fstack-check`: Stack overflow detection
- `-gnatVa`: Validity checking for all data
- `-gnata`: Assertions enabled

### Current Limitations

**⚠️  CRITICAL SECURITY WARNINGS:**

1. **Demo Credentials**: Hardcoded credentials with placeholder hashes
2. **Simplified Crypto**: Password comparison uses demo implementation
3. **No DNS Integration**: Cannot actually modify DNS servers yet
4. **No TSIG**: Secure DNS updates not implemented

**DO NOT deploy this to production without:**
- Replacing authentication with bcrypt/Argon2
- Implementing proper DNS UPDATE with TSIG
- Security audit and penetration testing
- Removing demo credentials

## 🎓 Use Cases

See [docs/USE_CASES.md](docs/USE_CASES.md) for detailed scenarios:

- Honeypot deployment obfuscation
- Incident response and threat hunting
- Security research and education
- Privacy enhancement for public services
- Testing and development environments
- "Quantum Server" demonstrations

## 📚 Technical Details

### DNS Records Supported

**HINFO (Host Information) - RFC 1035**
- CPU field: Hardware/architecture type
- OS field: Operating system name
- Deprecated per RFC 8482 (intentional!)

**LOC (Location) - RFC 1876**
- Latitude/Longitude in decimal degrees
- Altitude in meters above WGS84 ellipsoid
- Size and precision fields (horizontal/vertical)
- Deprecated per RFC 8482 (intentional!)

### Type Safety Examples

```ada
-- TTL must be 1-604800 seconds (enforced at compile time)
type TTL_Seconds is range 1 .. 604_800;

-- Latitude must be -90.0 to +90.0 degrees
type Latitude_Degrees is delta 0.000001 range -90.0 .. 90.0;

-- These guarantees cannot be violated - compiler prevents it!
```

### Permission Hierarchy

```
None < Read_Only < Modify_Local < Modify_Remote < Admin
```

Each level inherits permissions from lower levels.

## 🔬 Development

### Running Tests

```bash
make test           # Quick test build cycle
make verify         # SPARK formal verification
```

### Code Style

Follows standard Ada conventions:
- Types: `Mixed_Case_With_Underscores`
- Variables: `lower_case_with_underscores`
- Constants: `UPPER_CASE_WITH_UNDERSCORES`

### Contributing

1. Maintain type safety - don't use unchecked conversions
2. Keep security flags enabled
3. Document "why" not "what" in comments
4. Test with actual DNS servers when available

## 📋 Roadmap

### Phase 1: Core (Complete ✅)
- [x] DNS record types
- [x] Authentication system
- [x] Randomization engine
- [x] Interactive TUI
- [x] Build system
- [x] Sample data

### Phase 2: DNS Integration (In Progress)
- [ ] DNS UPDATE (RFC 2136) implementation
- [ ] TSIG authentication (RFC 2845)
- [ ] Zone file writer (BIND format)
- [ ] Test with real DNS servers

### Phase 3: Advanced Features
- [ ] Configuration file support
- [ ] Scheduled fluctuation (Ada tasking)
- [ ] Logging system
- [ ] API/webhook triggers
- [ ] Ncurses-based enhanced TUI

### Phase 4: Production Hardening
- [ ] bcrypt/Argon2 password hashing
- [ ] SPARK formal verification
- [ ] Security audit
- [ ] Unit test suite
- [ ] Performance optimization

## 🐛 Known Issues

1. **CSV Parser**: Simple implementation, doesn't handle quoted commas
2. **Demo Auth**: Uses placeholder hashes, not real crypto
3. **Chaos/Hesiod Classes**: Rarely supported by DNS servers
4. **No DNS Communication**: Cannot actually update servers yet

## 📄 License

[Specify your license here - MIT, GPL, etc.]

## 🙏 Acknowledgments

- RFCs 1034, 1035, 1876, 2136, 2845, 8482
- Ada community for security-focused language design
- GNAT compiler team for excellent tooling

## 📞 Contact

[Your contact information or GitHub profile]

---

## Fun Fact

The "Quantum Server" concept is inspired by quantum superposition - your server simultaneously exists as an Intel Xeon in San Francisco AND an ARM processor in Tokyo. Schrödinger would approve! 🐱⚛️

**Remember:** With great power comes great responsibility. Use this tool ethically and legally. Always obtain proper authorization before modifying DNS infrastructure.
