--  Master Configuration Specification
--  Comprehensive configuration for all enterprise features
--
--  Integrates:
--  - Extended DNS records (all types)
--  - Firewall management with time-based access
--  - Port rotation and service scheduling
--  - Security headers and obfuscation
--  - Software-Defined Perimeter (SDP)
--  - Protocol management (SNMP/NETCONF/etc.)
--  - DNS topology and AXFR
--  - APL-based access control

with DNS_Records;
with DNS_Records_Extended;
with Firewall_Manager;
with Security_Headers;
with SDP_Controller;
with Protocol_Manager;
with Config;  --  Original simple config

package Master_Config is

   --  ========================================================================
   --  DEPLOYMENT MODE
   --  ========================================================================

   type Deployment_Mode is (
      Development,      --  All features, verbose logging, diagnostics enabled
      Staging,          --  Production-like, testing enabled
      Production,       --  Maximum security, minimal exposure
      Honeypot,         --  Deception-focused configuration
      Research          --  Security research and experimentation
   );

   type Environment_Type is (
      Local_Development,
      CI_CD_Pipeline,
      Internal_Network,
      DMZ,
      Public_Internet,
      Air_Gapped        --  Isolated network
   );

   --  ========================================================================
   --  MASTER CONFIGURATION STRUCTURE
   --  ========================================================================

   type Master_Configuration is record
      --  Deployment settings
      Mode             : Deployment_Mode := Production;
      Environment      : Environment_Type := Public_Internet;
      Instance_Name    : DNS_Records_Extended.Short_Text;
      Config_Version   : DNS_Records_Extended.Short_Text;

      --  Legacy configuration (backward compatibility)
      Legacy_Config    : Config.Application_Config;

      --  DNS configuration
      DNS_Topology     : DNS_Records_Extended.DNS_Topology := DNS_Records_Extended.Standard;
      AXFR_Config      : DNS_Records_Extended.AXFR_Config;
      IP_Protocol_Mode : DNS_Records_Extended.IP_Protocol_Mode := DNS_Records_Extended.Dual_Stack;

      --  Original HINFO/LOC fluctuation
      HINFO_Fluctuation_Enabled : Boolean := True;
      LOC_Fluctuation_Enabled   : Boolean := True;
      Quantum_Server_Mode       : Boolean := True;  --  Both HINFO+LOC

      --  Extended DNS records management
      A_Records_Enabled      : Boolean := True;
      AAAA_Records_Enabled   : Boolean := True;
      MX_Records_Enabled     : Boolean := True;
      TXT_Records_Enabled    : Boolean := True;
      SPF_Enabled            : Boolean := True;
      DKIM_Enabled           : Boolean := True;
      DMARC_Enabled          : Boolean := True;
      CAA_Enabled            : Boolean := True;
      TLSA_Enabled           : Boolean := True;  --  DANE
      SSHFP_Enabled          : Boolean := True;
      APL_Enabled            : Boolean := True;  --  Access control
      DNSSEC_Enabled         : Boolean := False;  --  Complex, separate config

      --  Firewall and access control
      Firewall_Config  : Firewall_Manager.Firewalld_Config;
      Port_Rotation_Enabled : Boolean := True;

      --  Service scheduling
      MX_Scheduled     : Boolean := False;  --  Time-based mail acceptance
      RSS_Scheduled    : Boolean := False;  --  Time-based RSS availability
      NNTP_Scheduled   : Boolean := False;  --  Time-based NNTP access
      SSH_Port_Rotation : Boolean := True;   --  Rotate SSH port on schedule

      --  Security headers
      Security_Headers : Security_Headers.Security_Header_Set;
      Header_Obfuscation : Boolean := True;
      Diagnostic_Mode_Enabled : Boolean := True;  --  Maintainer diagnostics

      --  Software-Defined Perimeter
      SDP_Enabled      : Boolean := False;  --  Zero-trust access
      SDP_Config       : SDP_Controller.SDP_Controller_Config;

      --  Protocol management
      Protocol_Config  : Protocol_Manager.Protocol_Manager_Config;
      SNMP_Allowed     : Boolean := False;  --  Disabled by default (insecure)
      NETCONF_Preferred : Boolean := True;   --  Use NETCONF over SNMP

      --  Maintenance and operations
      Maintenance_Windows : Firewall_Manager.Maintenance_Window_Ptr;
      Emergency_Contact   : DNS_Records_Extended.Medium_Text;
      Backup_Enabled      : Boolean := True;
      Backup_Interval     : Duration := 86400.0;  --  Daily

      --  Logging and monitoring
      Audit_Logging    : Boolean := True;
      Security_Logging : Boolean := True;
      Metrics_Export   : Boolean := True;
      Syslog_Server    : DNS_Records_Extended.Short_Text;

      --  Integration hooks
      Custom_Scripts   : DNS_Records_Extended.Very_Long_Text;  --  Pre/post hooks
      Webhook_URL      : DNS_Records_Extended.Medium_Text;
      API_Enabled      : Boolean := False;
   end record;

   --  ========================================================================
   --  CONFIGURATION PROFILES (Presets)
   --  ========================================================================

   --  Get predefined configuration for deployment mode
   function Get_Default_Config (Mode : Deployment_Mode) return Master_Configuration;

   --  Development profile
   function Development_Profile return Master_Configuration;

   --  Staging profile
   function Staging_Profile return Master_Configuration;

   --  Production profile (maximum security)
   function Production_Profile return Master_Configuration;

   --  Honeypot profile (deception-focused)
   function Honeypot_Profile return Master_Configuration;

   --  Research profile (experimental features)
   function Research_Profile return Master_Configuration;

   --  ========================================================================
   --  CONFIGURATION VALIDATION
   --  ========================================================================

   --  Validate configuration for security issues
   type Validation_Result is record
      Valid           : Boolean;
      Warnings_Count  : Natural;
      Errors_Count    : Natural;
      Messages        : DNS_Records_Extended.Very_Long_Text;
   end record;

   function Validate_Configuration (
      Config : Master_Configuration
   ) return Validation_Result;

   --  Check for insecure settings
   function Check_Security_Posture (
      Config : Master_Configuration
   ) return String;  --  Returns report

   --  ========================================================================
   --  CONFIGURATION FILE OPERATIONS
   --  ========================================================================

   --  Load from extended INI format
   procedure Load_Master_Config (
      Filename : String;
      Config   : out Master_Configuration
   );

   --  Save to extended INI format
   procedure Save_Master_Config (
      Filename : String;
      Config   : Master_Configuration
   );

   --  Import from YAML (if parser available)
   procedure Import_From_YAML (
      Filename : String;
      Config   : out Master_Configuration
   );

   --  Export to YAML
   function Export_To_YAML (
      Config : Master_Configuration
   ) return String;

   --  Export to JSON (for API consumption)
   function Export_To_JSON (
      Config : Master_Configuration
   ) return String;

   --  ========================================================================
   --  CONFIGURATION TEMPLATES
   --  ========================================================================

   --  Generate example configuration with comments
   function Generate_Example_Config return String;

   --  Generate configuration for specific scenario
   function Generate_Scenario_Config (
      Scenario : String  --  "public-web", "internal-app", "honeypot", etc.
   ) return Master_Configuration;

   --  ========================================================================
   --  RUNTIME CONFIGURATION UPDATES
   --  ========================================================================

   --  Apply configuration changes without restart
   procedure Apply_Configuration_Hot (
      Config : Master_Configuration
   );

   --  Reload configuration from file
   procedure Reload_Configuration (
      Filename : String
   );

   --  Get current active configuration
   function Get_Active_Configuration return Master_Configuration;

   --  ========================================================================
   --  MIGRATION AND COMPATIBILITY
   --  ========================================================================

   --  Migrate from old config format
   function Migrate_From_Simple_Config (
      Old_Config : Config.Application_Config
   ) return Master_Configuration;

   --  Check configuration version compatibility
   function Is_Compatible (
      Config_Version : String
   ) return Boolean;

   --  ========================================================================
   --  CONFIGURATION DIFF AND COMPARISON
   --  ========================================================================

   --  Compare two configurations
   function Compare_Configurations (
      Config_A : Master_Configuration;
      Config_B : Master_Configuration
   ) return String;  --  Returns diff report

   --  Generate migration plan
   function Generate_Migration_Plan (
      From_Config : Master_Configuration;
      To_Config   : Master_Configuration
   ) return String;

   --  ========================================================================
   --  EMERGENCY CONFIGURATIONS
   --  ========================================================================

   --  Emergency lockdown configuration
   function Emergency_Lockdown_Config return Master_Configuration;

   --  Emergency recovery configuration
   function Emergency_Recovery_Config return Master_Configuration;

   --  Minimal safe configuration
   function Minimal_Safe_Config return Master_Configuration;

   --  ========================================================================
   --  EXCEPTIONS
   --  ========================================================================

   Config_Validation_Error : exception;
   Config_Load_Error : exception;
   Config_Apply_Error : exception;
   Insecure_Config_Error : exception;

end Master_Config;
