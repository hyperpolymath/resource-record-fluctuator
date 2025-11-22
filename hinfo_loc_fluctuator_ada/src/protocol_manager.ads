--  Protocol Management Specification
--  Support for SNMP and safer alternatives (NETCONF, RESTCONF, gNMI)
--
--  Protocols supported:
--  - SNMP v1/v2c/v3 (with security warnings)
--  - NETCONF (RFC 6241) - Recommended over SNMP
--  - RESTCONF (RFC 8040) - RESTful NETCONF
--  - gNMI (gRPC Network Management Interface) - Modern alternative
--  - Prometheus/OpenMetrics - For metrics export

with DNS_Records_Extended;
with Ada.Containers.Vectors;

package Protocol_Manager is

   --  ========================================================================
   --  PROTOCOL TYPES
   --  ========================================================================

   type Management_Protocol is (
      SNMP_v1,       --  ⚠️  Insecure - plaintext community strings
      SNMP_v2c,      --  ⚠️  Insecure - plaintext community strings
      SNMP_v3,       --  ✅ Secure - encryption and authentication
      NETCONF,       --  ✅ Recommended - XML-based over SSH
      RESTCONF,      --  ✅ Recommended - RESTful API over HTTPS
      gNMI,          --  ✅ Modern - gRPC-based with strong auth
      Prometheus,    --  ✅ Pull model - metrics endpoint
      OpenMetrics,   --  ✅ Standard metrics format
      Custom_API     --  Custom REST/gRPC API
   );

   --  Protocol security level
   type Security_Level is (
      Insecure,      --  SNMPv1, SNMPv2c
      Basic,         --  Basic auth over TLS
      Strong,        --  Certificate-based mutual TLS
      Zero_Trust     --  SDP-integrated with continuous verification
   );

   --  ========================================================================
   --  SNMP CONFIGURATION (with security warnings)
   --  ========================================================================

   --  SNMP version configuration
   type SNMP_Version_Config is record
      Version          : Management_Protocol;  --  SNMP_v1, v2c, or v3
      Enabled          : Boolean := False;  --  Disabled by default
      Community_String : DNS_Records_Extended.Short_Text;  --  v1/v2c only
      Port             : DNS_Records_Extended.Port_Number := 161;

      --  SNMPv3 security
      Security_Name    : DNS_Records_Extended.Short_Text;
      Auth_Protocol    : DNS_Records_Extended.Short_Text;  --  "MD5", "SHA", "SHA-256"
      Auth_Password    : DNS_Records_Extended.Medium_Text;
      Priv_Protocol    : DNS_Records_Extended.Short_Text;  --  "DES", "AES128", "AES256"
      Priv_Password    : DNS_Records_Extended.Medium_Text;

      --  Access control
      Allowed_IPs      : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Read_Only        : Boolean := True;
      Time_Windows     : Firewall_Manager.Maintenance_Window_Ptr;  --  Time-based access
   end record;

   --  SNMP configuration
   type SNMP_Config is record
      Enabled          : Boolean := False;
      Versions         : array (SNMP_v1 .. SNMP_v3) of SNMP_Version_Config;
      System_Contact   : DNS_Records_Extended.Short_Text;
      System_Location  : DNS_Records_Extended.Short_Text;
      System_Name      : DNS_Records_Extended.Short_Text;
      Traps_Enabled    : Boolean := False;
      Trap_Destination : DNS_Records_Extended.Short_Text;

      --  Security recommendations
      Warn_Insecure    : Boolean := True;  --  Warn about v1/v2c
      Require_v3_Only  : Boolean := True;  --  Enforce SNMPv3
   end record;

   --  ========================================================================
   --  NETCONF CONFIGURATION (Recommended)
   --  ========================================================================

   --  NETCONF capabilities
   type NETCONF_Capability is (
      Base_1_0,          --  RFC 4741
      Base_1_1,          --  RFC 6241
      Candidate,         --  Candidate configuration
      Confirmed_Commit,  --  Two-phase commit
      Rollback_On_Error, --  Automatic rollback
      Validate,          --  Configuration validation
      Startup,           --  Startup configuration
      URL,               --  URL-based configuration
      XPath,             --  XPath filtering
      Notification,      --  Event notifications
      Interleave         --  Interleaved notifications
   );

   package Capability_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => NETCONF_Capability
   );

   type NETCONF_Config is record
      Enabled          : Boolean := True;  --  Recommended default
      Port             : DNS_Records_Extended.Port_Number := 830;  --  NETCONF over SSH
      SSH_Host_Key     : DNS_Records_Extended.Very_Long_Text;
      Capabilities     : Capability_Vectors.Vector;

      --  Authentication
      Auth_Method      : DNS_Records_Extended.Short_Text;  --  "password", "publickey", "keyboard-interactive"
      Allowed_Users    : DNS_Records_Extended.Medium_Text;  --  Comma-separated
      Require_MFA      : Boolean := True;

      --  Access control
      Allowed_IPs      : DNS_Records_Extended.APL_Entry_Array_Ptr;
      TLS_Enabled      : Boolean := True;  --  NETCONF over TLS (RFC 7589)
      Certificate      : DNS_Records_Extended.Very_Long_Text;

      --  Session management
      Max_Sessions     : Natural := 10;
      Session_Timeout  : Duration := 600.0;  --  10 minutes
      Idle_Timeout     : Duration := 300.0;  --  5 minutes
   end record;

   --  ========================================================================
   --  RESTCONF CONFIGURATION (Recommended)
   --  ========================================================================

   --  RESTCONF authentication method
   type RESTCONF_Auth_Method is (
      HTTP_Basic,        --  ⚠️  Only over HTTPS!
      Bearer_Token,      --  JWT or OAuth2
      Client_Certificate, --  Mutual TLS
      API_Key,           --  Custom API key
      OAuth2             --  Full OAuth2 flow
   );

   type RESTCONF_Config is record
      Enabled          : Boolean := True;  --  Recommended default
      Port             : DNS_Records_Extended.Port_Number := 443;  --  HTTPS
      Base_Path        : DNS_Records_Extended.Short_Text;  --  e.g., "/restconf"

      --  Authentication
      Auth_Method      : RESTCONF_Auth_Method := Client_Certificate;
      API_Keys         : DNS_Records_Extended.Very_Long_Text;  --  List of valid keys
      OAuth2_Issuer    : DNS_Records_Extended.Medium_Text;
      OAuth2_Audience  : DNS_Records_Extended.Medium_Text;

      --  TLS/SSL
      TLS_Version      : DNS_Records_Extended.Short_Text;  --  "1.2", "1.3"
      Certificate      : DNS_Records_Extended.Very_Long_Text;
      Private_Key      : DNS_Records_Extended.Very_Long_Text;
      Client_CA        : DNS_Records_Extended.Very_Long_Text;  --  For client cert validation

      --  Access control
      Allowed_IPs      : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Rate_Limit       : Natural := 100;  --  Requests per minute
      CORS_Enabled     : Boolean := False;
      CORS_Origins     : DNS_Records_Extended.Medium_Text;

      --  Data formats
      Accept_JSON      : Boolean := True;
      Accept_XML       : Boolean := True;
      Default_Format   : DNS_Records_Extended.Short_Text;  --  "json" or "xml"
   end record;

   --  ========================================================================
   --  gNMI CONFIGURATION (Modern alternative)
   --  ========================================================================

   --  gNMI encoding
   type gNMI_Encoding is (
      JSON,
      JSON_IETF,
      Bytes,
      Proto,
      ASCII
   );

   type gNMI_Config is record
      Enabled          : Boolean := True;  --  Modern default
      Port             : DNS_Records_Extended.Port_Number := 9339;  --  Standard gNMI port

      --  gRPC/TLS
      TLS_Enabled      : Boolean := True;  --  Always recommended
      Certificate      : DNS_Records_Extended.Very_Long_Text;
      Private_Key      : DNS_Records_Extended.Very_Long_Text;
      Client_CA        : DNS_Records_Extended.Very_Long_Text;

      --  Authentication
      Username         : DNS_Records_Extended.Short_Text;
      Password         : DNS_Records_Extended.Medium_Text;
      Token_Auth       : Boolean := True;  --  JWT tokens

      --  gNMI capabilities
      Encoding         : gNMI_Encoding := JSON_IETF;
      Subscribe        : Boolean := True;  --  Streaming telemetry
      Get_Enabled      : Boolean := True;
      Set_Enabled      : Boolean := False;  --  Read-only by default

      --  Access control
      Allowed_IPs      : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Max_Connections  : Natural := 50;
   end record;

   --  ========================================================================
   --  PROMETHEUS/OPENMETRICS CONFIGURATION
   --  ========================================================================

   type Metrics_Config is record
      Enabled          : Boolean := True;
      Protocol         : Management_Protocol := Prometheus;  --  or OpenMetrics
      Port             : DNS_Records_Extended.Port_Number := 9090;
      Metrics_Path     : DNS_Records_Extended.Short_Text;  --  "/metrics"

      --  Authentication (for pulling metrics)
      Auth_Required    : Boolean := True;
      Auth_Token       : DNS_Records_Extended.Medium_Text;  --  Bearer token
      Client_Certs     : Boolean := True;  --  Mutual TLS

      --  Access control
      Allowed_IPs      : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Rate_Limit       : Natural := 60;  --  Scrapes per minute

      --  Metrics exposure
      Expose_System    : Boolean := True;
      Expose_Network   : Boolean := True;
      Expose_Application : Boolean := True;
      Custom_Metrics   : DNS_Records_Extended.Very_Long_Text;
   end record;

   --  ========================================================================
   --  PROTOCOL MANAGER CONFIGURATION
   --  ========================================================================

   type Protocol_Manager_Config is record
      --  Legacy (insecure) protocols
      SNMP             : SNMP_Config;

      --  Recommended secure protocols
      NETCONF          : NETCONF_Config;
      RESTCONF         : RESTCONF_Config;
      gNMI             : gNMI_Config;

      --  Metrics export
      Metrics          : Metrics_Config;

      --  Security policy
      Prefer_Secure    : Boolean := True;  --  Favor NETCONF/RESTCONF/gNMI
      Disable_Insecure : Boolean := True;  --  Disable SNMPv1/v2c
      Require_TLS      : Boolean := True;  --  All protocols must use TLS
      SDP_Integration  : Boolean := True;  --  Integrate with SDP controller

      --  Time-based availability
      Scheduled_Access : Firewall_Manager.Maintenance_Window_Ptr;
   end record;

   --  ========================================================================
   --  PROTOCOL OPERATIONS
   --  ========================================================================

   --  Initialize protocol manager
   procedure Initialize (Config : Protocol_Manager_Config);

   --  Enable/disable specific protocols
   procedure Enable_Protocol (Protocol : Management_Protocol);
   procedure Disable_Protocol (Protocol : Management_Protocol);

   --  Start/stop protocol servers
   procedure Start_SNMP_Agent;
   procedure Stop_SNMP_Agent;
   procedure Start_NETCONF_Server;
   procedure Stop_NETCONF_Server;
   procedure Start_RESTCONF_Server;
   procedure Stop_RESTCONF_Server;
   procedure Start_gNMI_Server;
   procedure Stop_gNMI_Server;

   --  Security validation
   function Validate_SNMP_Config (Config : SNMP_Config) return Boolean;
   function Get_Security_Level (Protocol : Management_Protocol) return Security_Level;

   --  Get protocol recommendations
   function Get_Protocol_Recommendation return Management_Protocol;
   function Get_Security_Warnings return String;

   --  ========================================================================
   --  INTEGRATION WITH FIREWALL/SDP
   --  ========================================================================

   --  Apply firewall rules for protocol access
   procedure Apply_Protocol_Firewall_Rules (
      Protocol : Management_Protocol;
      Config   : Protocol_Manager_Config
   );

   --  Integrate with SDP for zero-trust access
   procedure Enable_SDP_Integration (
      Protocol : Management_Protocol;
      Require_Session : Boolean := True
   );

   --  ========================================================================
   --  MONITORING AND METRICS
   --  ========================================================================

   --  Get protocol statistics
   type Protocol_Stats is record
      Protocol         : Management_Protocol;
      Enabled          : Boolean;
      Active_Sessions  : Natural;
      Total_Requests   : Natural;
      Failed_Auth      : Natural;
      Last_Access      : Ada.Calendar.Time;
      Security_Level   : Security_Level;
   end record;

   function Get_Protocol_Stats (Protocol : Management_Protocol) return Protocol_Stats;
   function Get_All_Protocol_Stats return String;

   --  Export metrics in Prometheus format
   function Export_Prometheus_Metrics return String;

   --  Exceptions
   Protocol_Error : exception;
   Insecure_Protocol_Error : exception;

end Protocol_Manager;
