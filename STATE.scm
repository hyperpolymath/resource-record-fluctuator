;;; STATE.scm - HINFO-LOC Fluctuator Project State
;;;
;;; Guile Scheme checkpoint file for AI conversation context preservation.
;;; Download at end of session -> Upload at start of next.
;;;
;;; Format: state.scm (https://github.com/hyperpolymath/state.scm)
;;; SPDX-License-Identifier: MIT

(define state
  `((metadata
     . ((format-version . "1.0")
        (schema-version . "2024-01")
        (created . "2025-12-08")
        (last-updated . "2025-12-08")
        (project-name . "HINFO-LOC Fluctuator")
        (repository . "hyperpolymath/resource-record-fluctuator")))

    (user
     . ((name . "hyperpolymath")
        (roles . (developer maintainer))
        (language-preferences . (ada scheme guix))
        (tool-preferences . (gnat make git guix))
        (values . (security-first type-safety formal-verification))))

    (session
     . ((conversation-id . "01Dn945CbdTGQRq4UXES5rnB")
        (start-time . "2025-12-08")
        (message-count . 1)
        (branch . "claude/create-state-scm-01Dn945CbdTGQRq4UXES5rnB")))

    (focus
     . ((current-project . "hinfo-loc-fluctuator")
        (current-phase . "implementation-validation")
        (deadline . #f)
        (blocking . (crypto-library-integration dns-server-testing))))

    (projects
     . (((name . "hinfo-loc-fluctuator")
         (status . in-progress)
         (completion . 65)
         (category . dns-security-tool)
         (phase . "mvp-v1-implementation")
         (description . "DNS record randomization for HINFO/LOC with enterprise security features")
         (tech-stack . (ada-2012 gnat spark))
         (dependencies . (gnat-compiler make))
         (blockers . (demo-credentials-must-be-replaced
                      crypto-library-needed
                      dns-server-integration-untested))
         (next-actions . (test-compilation
                          complete-csv-parser
                          replace-demo-auth
                          integrate-bcrypt-or-argon2
                          test-dns-update)))))

    ;;; =========================================================
    ;;; CURRENT POSITION
    ;;; =========================================================
    (current-position
     . ((summary . "Enterprise Ada specifications complete; implementation bodies need validation and crypto integration")

        (completed-work
         . ((ada-specifications . "15 .ads files complete with full type definitions")
            (ada-bodies . "16 .adb files present - frameworks/stubs")
            (build-system . "GNAT project file + Makefile with security flags")
            (sample-data . "68 CPUs, 77 OSes, 52 locations")
            (documentation . "README, USE_CASES, ENTERPRISE_FEATURES, CLAUDE.md")))

        (module-status
         . ((core-complete
             . (dns-records secure-auth randomizer tui main))
            (advanced-frameworks
             . (zone-writer logger config scheduler dns-update))
            (enterprise-specs
             . (dns-records-extended firewall-manager security-headers
                sdp-controller protocol-manager master-config))))

        (code-metrics
         . ((total-lines . "~12,300")
            (ada-specs . "~4,500 lines")
            (ada-bodies . "~3,000 lines")
            (documentation . "~4,800 lines")))))

    ;;; =========================================================
    ;;; ROUTE TO MVP v1
    ;;; =========================================================
    (mvp-v1-roadmap
     . ((version . "1.0.0-mvp")
        (goal . "Functional HINFO/LOC fluctuation with secure auth and zone file output")

        (phase-1-compilation-validation
         . ((priority . 1)
            (status . pending)
            (tasks . ("Install GNAT compiler if not present"
                      "Run 'make' in hinfo_loc_fluctuator_ada/"
                      "Fix any compilation errors"
                      "Verify all 16 modules compile cleanly"))))

        (phase-2-csv-parser-completion
         . ((priority . 2)
            (status . pending)
            (tasks . ("Complete Load_Location_Pool in randomizer.adb"
                      "Parse data/locations.csv format: lat,lon,alt,description"
                      "Add error handling for malformed CSV"
                      "Test with 52 sample locations"))))

        (phase-3-authentication-hardening
         . ((priority . 3)
            (status . pending)
            (tasks . ("Integrate bcrypt or Argon2 library"
                      "Remove hardcoded demo credentials"
                      "Implement secure password storage"
                      "Test authentication flow"))))

        (phase-4-zone-file-output
         . ((priority . 4)
            (status . pending)
            (tasks . ("Test BIND zone file generation"
                      "Verify SOA record formatting"
                      "Test atomic file updates"
                      "Validate with named-checkzone"))))

        (phase-5-end-to-end-testing
         . ((priority . 5)
            (status . pending)
            (tasks . ("Run complete TUI workflow"
                      "Test all menu options"
                      "Verify randomization output"
                      "Document any remaining issues"))))))

    ;;; =========================================================
    ;;; KNOWN ISSUES
    ;;; =========================================================
    (issues
     . (((id . "ISSUE-001")
         (severity . critical)
         (title . "Demo credentials in production code")
         (description . "secure_auth.adb contains hardcoded admin/user demo accounts with placeholder hashes")
         (impact . "Security vulnerability if deployed without modification")
         (resolution . "Replace with bcrypt/Argon2 and remove demo accounts")
         (status . open))

        ((id . "ISSUE-002")
         (severity . high)
         (title . "No real cryptography")
         (description . "Password comparison uses demo implementation, not cryptographically secure hashing")
         (impact . "Authentication can be bypassed or credentials leaked")
         (resolution . "Integrate proper crypto library (bcrypt or Argon2 recommended)")
         (status . open))

        ((id . "ISSUE-003")
         (severity . medium)
         (title . "CSV parser stubbed")
         (description . "Load_Location_Pool reads filename but doesn't parse CSV content")
         (impact . "Location randomization uses only hardcoded fallback data")
         (resolution . "Implement CSV parsing in randomizer.adb")
         (status . open))

        ((id . "ISSUE-004")
         (severity . medium)
         (title . "Enterprise modules untested")
         (description . "Six enterprise .ads/.adb pairs (firewall, sdp, etc.) have specs but bodies may be stubs")
         (impact . "Enterprise features not functional until bodies completed")
         (resolution . "Complete implementation bodies and unit test")
         (status . open))

        ((id . "ISSUE-005")
         (severity . low)
         (title . "No DNS server integration testing")
         (description . "DNS UPDATE and zone file writer exist but haven't been tested against real server")
         (impact . "Unknown compatibility with BIND/PowerDNS/NSD")
         (resolution . "Set up test DNS server and validate integration")
         (status . open))

        ((id . "ISSUE-006")
         (severity . low)
         (title . "Chaos/Hesiod DNS classes")
         (description . "CH and HS classes included for completeness but rarely supported")
         (impact . "Minor - only IN class useful in practice")
         (resolution . "Document limitation; low priority")
         (status . wontfix))))

    ;;; =========================================================
    ;;; QUESTIONS FOR USER
    ;;; =========================================================
    (questions
     . (((id . "Q-001")
         (priority . high)
         (question . "Which DNS server should be the primary target for testing?")
         (options . ("BIND 9 (most common)"
                     "PowerDNS (modern, API-friendly)"
                     "NSD (authoritative-only)"
                     "All three"))
         (context . "Affects DNS UPDATE wire format and zone file format priorities")
         (status . pending))

        ((id . "Q-002")
         (priority . high)
         (question . "Priority: security hardening vs feature completion?")
         (options . ("Security first - replace demo auth before anything else"
                     "Features first - get basic functionality working"
                     "Parallel - work on both"))
         (context . "Demo credentials are a security risk but features need testing")
         (status . pending))

        ((id . "Q-003")
         (priority . medium)
         (question . "Preferred cryptographic library for password hashing?")
         (options . ("bcrypt (proven, widely used)"
                     "Argon2 (modern, memory-hard)"
                     "GNAT.SHA (built-in but weaker)"))
         (context . "Affects secure_auth.adb implementation")
         (status . pending))

        ((id . "Q-004")
         (priority . medium)
         (question . "TUI approach: enhance current or implement ncurses?")
         (options . ("Keep simple text TUI (working now)"
                     "Implement ncurses TUI (better UX, more work)"))
         (context . "Current text TUI functional; ncurses requires binding")
         (status . pending))

        ((id . "Q-005")
         (priority . low)
         (question . "Should enterprise features be in MVP v1 or deferred to v2?")
         (options . ("MVP v1 - core HINFO/LOC only"
                     "MVP v1.5 - include basic enterprise (firewall, headers)"
                     "MVP v2 - full enterprise platform"))
         (context . "Enterprise specs exist but bodies need completion")
         (status . pending))))

    ;;; =========================================================
    ;;; LONG-TERM ROADMAP
    ;;; =========================================================
    (roadmap
     . ((v1-mvp
         . ((version . "1.0.0")
            (status . in-progress)
            (description . "Core HINFO/LOC fluctuation with secure auth")
            (features . ("Working TUI"
                         "Secure authentication (bcrypt/Argon2)"
                         "HINFO record randomization"
                         "LOC record randomization"
                         "BIND zone file output"
                         "Basic scheduling"))))

        (v1-1-dns-integration
         . ((version . "1.1.0")
            (status . planned)
            (description . "Real DNS server integration")
            (features . ("DNS UPDATE (RFC 2136)"
                         "TSIG authentication (RFC 2845)"
                         "PowerDNS API support"
                         "Atomic zone file updates"))))

        (v1-2-testing
         . ((version . "1.2.0")
            (status . planned)
            (description . "Testing and validation")
            (features . ("Unit test suite"
                         "Integration tests"
                         "Performance benchmarks"
                         "Security audit"))))

        (v2-enterprise
         . ((version . "2.0.0")
            (status . planned)
            (description . "Enterprise security platform")
            (features . ("Extended DNS records (20+ types)"
                         "Firewall integration"
                         "Port rotation"
                         "Security header obfuscation"
                         "Service scheduling"))))

        (v2-1-zero-trust
         . ((version . "2.1.0")
            (status . planned)
            (description . "Zero-trust access control")
            (features . ("SDP controller"
                         "Single Packet Authorization"
                         "Device posture validation"
                         "Continuous authentication"))))

        (v2-2-management
         . ((version . "2.2.0")
            (status . planned)
            (description . "Modern management protocols")
            (features . ("NETCONF support"
                         "RESTCONF API"
                         "gNMI integration"
                         "Prometheus metrics"))))

        (v3-verification
         . ((version . "3.0.0")
            (status . future)
            (description . "Formal verification and certification")
            (features . ("SPARK formal proofs"
                         "Security certification (Common Criteria)"
                         "Compliance documentation"))))

        (v3-1-web-ui
         . ((version . "3.1.0")
            (status . future)
            (description . "Web-based management")
            (features . ("Web dashboard"
                         "Real-time monitoring"
                         "Configuration editor"
                         "Audit log viewer"))))

        (v4-enterprise-plus
         . ((version . "4.0.0")
            (status . future)
            (description . "Enterprise+ features")
            (features . ("Clustering support"
                         "High availability"
                         "Multi-tenancy"
                         "Plugin system"))))))

    ;;; =========================================================
    ;;; CRITICAL NEXT ACTIONS
    ;;; =========================================================
    (critical-next
     . (((priority . 1)
         (action . "Test compilation: run 'make' in hinfo_loc_fluctuator_ada/")
         (deadline . #f)
         (context . "Verify all 16 Ada modules compile without errors"))

        ((priority . 2)
         (action . "Complete CSV parser in randomizer.adb")
         (deadline . #f)
         (context . "Enable loading of 52 sample locations from locations.csv"))

        ((priority . 3)
         (action . "Replace demo authentication credentials")
         (deadline . #f)
         (context . "SECURITY: demo admin/user accounts must not reach production"))

        ((priority . 4)
         (action . "Integrate bcrypt or Argon2 library")
         (deadline . #f)
         (context . "Required for secure password storage"))

        ((priority . 5)
         (action . "End-to-end TUI test")
         (deadline . #f)
         (context . "Verify complete workflow: login -> randomize -> output"))))

    ;;; =========================================================
    ;;; HISTORY / VELOCITY TRACKING
    ;;; =========================================================
    (history
     . (((date . "2025-12-08")
         (snapshot . ((total-completion . 65)
                      (modules-complete . 10)
                      (modules-framework . 6)
                      (issues-open . 6)
                      (issues-closed . 0)))
         (notes . "Initial STATE.scm created; project analysis complete"))))

    ;;; =========================================================
    ;;; FILES MODIFIED THIS SESSION
    ;;; =========================================================
    (files-created-this-session
     . ("STATE.scm"))

    (files-modified-this-session
     . ())

    ;;; =========================================================
    ;;; CONTEXT NOTES
    ;;; =========================================================
    (context-notes
     . (("HINFO and LOC records are intentionally deprecated (RFC 8482) - this is a feature, not a bug")
        ("Ada was chosen over Elixir for security/type-safety reasons - do not suggest rewrites")
        ("The 'quantum server' humor is intentional - project has both serious and playful aspects")
        ("Enterprise features were added to expand scope from hobby tool to security platform")
        ("All compiler safety flags remain enabled even in release builds - this is deliberate")
        ("Type constraints (e.g., Latitude_Degrees range -90..90) are enforced at compile time")))))

;;; Quick reference for state queries
;;;
;;; Get current focus:
;;;   (assoc-ref (assoc-ref state 'focus) 'current-project)
;;;
;;; Get all blockers:
;;;   (assoc-ref (assoc-ref state 'focus) 'blocking)
;;;
;;; Get project status:
;;;   (assoc-ref (car (assoc-ref state 'projects)) 'status)
;;;
;;; Get critical next actions:
;;;   (assoc-ref state 'critical-next)
;;;
;;; Get open issues:
;;;   (filter (lambda (i) (eq? (assoc-ref i 'status) 'open))
;;;           (assoc-ref state 'issues))

;;; end STATE.scm
