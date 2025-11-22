--  Software-Defined Perimeter (SDP) Controller Specification
--  Zero-trust network access with dynamic perimeter enforcement
--
--  Implements Cloud Security Alliance SDP architecture:
--  - Single Packet Authorization (SPA)
--  - Dynamic firewall rules
--  - Identity-based access
--  - Device posture validation
--  - Micro-segmentation
--
--  References:
--  - CSA SDP Specification v2.0
--  - NIST Zero Trust Architecture (SP 800-207)
--  - fwknop (Single Packet Authorization reference)

with DNS_Records_Extended;
with Firewall_Manager;
with Ada.Calendar;
with Interfaces;

package SDP_Controller is

   --  ========================================================================
   --  SDP ARCHITECTURE COMPONENTS
   --  ========================================================================

   --  SDP component types
   type SDP_Component is (
      SDP_Controller,      --  Central policy engine
      SDP_Gateway,         --  Enforcement point (firewall)
      SDP_Client,          --  Authenticated endpoint
      SDP_Policy_Engine    --  Policy decision point
   );

   --  ========================================================================
   --  SINGLE PACKET AUTHORIZATION (SPA)
   --  ========================================================================

   --  SPA encryption algorithm
   type SPA_Algorithm is (
      AES_256_CBC,
      AES_256_GCM,
      ChaCha20_Poly1305
   );

   --  SPA HMAC algorithm
   type SPA_HMAC_Algorithm is (
      HMAC_SHA256,
      HMAC_SHA512,
      HMAC_SHA3_256
   );

   --  SPA packet structure (encrypted)
   type SPA_Packet is record
      Version          : Interfaces.Unsigned_8 := 1;
      Random_Data      : DNS_Records_Extended.Medium_Text;  --  Anti-replay
      Username         : DNS_Records_Extended.Short_Text;
      Timestamp        : Ada.Calendar.Time;
      Requested_Access : DNS_Records_Extended.Short_Text;  --  Service/port
      Client_IP        : DNS_Records_Extended.Short_Text;
      Device_ID        : DNS_Records_Extended.Medium_Text;
      HMAC             : DNS_Records_Extended.Medium_Text;
   end record;

   --  SPA configuration
   type SPA_Config is record
      Enabled           : Boolean := True;
      Encryption        : SPA_Algorithm := AES_256_GCM;
      HMAC_Algorithm    : SPA_HMAC_Algorithm := HMAC_SHA256;
      Shared_Key        : DNS_Records_Extended.Medium_Text;  --  Pre-shared key
      Replay_Window     : Duration := 60.0;  --  Allow 60s clock skew
      Max_Attempts      : Natural := 3;
      Lockout_Duration  : Duration := 300.0;  --  5 minutes
   end record;

   --  ========================================================================
   --  ZERO TRUST POLICY
   --  ========================================================================

   --  Trust level (continuous verification)
   type Trust_Level is (
      Untrusted,        --  Default state
      Device_Verified,  --  Device certificate valid
      User_Authenticated, --  User authenticated
      Posture_Valid,    --  Device posture check passed
      Full_Trust        --  All checks passed
   );

   --  Device posture requirements
   type Posture_Requirements is record
      OS_Version_Min    : DNS_Records_Extended.Short_Text;
      Antivirus_Required : Boolean := True;
      Firewall_Required  : Boolean := True;
      Disk_Encrypted     : Boolean := True;
      Patch_Level_Days   : Natural := 30;  --  Max days since last patch
      Custom_Checks      : DNS_Records_Extended.Very_Long_Text;  --  Script/policy
   end record;

   --  Access policy
   type Access_Policy is record
      Policy_Name       : DNS_Records_Extended.Short_Text;
      Required_Trust    : Trust_Level := Full_Trust;
      Allowed_Users     : DNS_Records_Extended.Medium_Text;  --  User list
      Allowed_Groups    : DNS_Records_Extended.Medium_Text;  --  Group list
      Allowed_Devices   : DNS_Records_Extended.Medium_Text;  --  Device IDs
      Time_Restrictions : Firewall_Manager.Time_Window;
      Source_CIDR       : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Destination       : DNS_Records_Extended.Short_Text;  --  Service/resource
      Ports             : Firewall_Manager.Port_Array_Ptr;
      Protocol          : DNS_Records_Extended.Short_Text;  --  tcp, udp
      Action            : Firewall_Manager.Rule_Action := Firewall_Manager.Accept;
      Session_Duration  : Duration := 3600.0;  --  1 hour default
      Continuous_Verify : Boolean := True;  --  Re-check periodically
   end record;

   type Access_Policy_Array is array (Positive range <>) of Access_Policy;
   type Access_Policy_Ptr is access Access_Policy_Array;

   --  ========================================================================
   --  IDENTITY AND DEVICE MANAGEMENT
   --  ========================================================================

   --  Device certificate/identity
   type Device_Identity is record
      Device_ID         : DNS_Records_Extended.Medium_Text;  --  Unique ID
      Certificate       : DNS_Records_Extended.Very_Long_Text;  --  X.509 cert
      Public_Key        : DNS_Records_Extended.Very_Long_Text;
      Registered        : Ada.Calendar.Time;
      Last_Seen         : Ada.Calendar.Time;
      Trust_Level       : Trust_Level := Untrusted;
      Posture_Status    : Boolean := False;
      Owner_User        : DNS_Records_Extended.Short_Text;
   end record;

   --  User identity with MFA
   type User_Identity is record
      Username          : DNS_Records_Extended.Short_Text;
      Groups            : DNS_Records_Extended.Medium_Text;  --  Comma-separated
      MFA_Enabled       : Boolean := True;
      MFA_Type          : DNS_Records_Extended.Short_Text;  --  "totp", "u2f", "webauthn"
      Certificate       : DNS_Records_Extended.Very_Long_Text;
      Last_Login        : Ada.Calendar.Time;
      Failed_Attempts   : Natural := 0;
      Locked_Until      : Ada.Calendar.Time;
   end record;

   --  ========================================================================
   --  SESSION MANAGEMENT
   --  ========================================================================

   --  Active SDP session
   type SDP_Session is record
      Session_ID        : DNS_Records_Extended.Medium_Text;  --  UUID
      User              : User_Identity;
      Device            : Device_Identity;
      Source_IP         : DNS_Records_Extended.Short_Text;
      Destination       : DNS_Records_Extended.Short_Text;
      Ports             : Firewall_Manager.Port_Array_Ptr;
      Established       : Ada.Calendar.Time;
      Expires           : Ada.Calendar.Time;
      Last_Activity     : Ada.Calendar.Time;
      Firewall_Rule     : Firewall_Manager.Firewall_Rule;
      Continuous_Checks : Boolean := True;
      Valid             : Boolean := True;
   end record;

   type SDP_Session_Array is array (Positive range <>) of SDP_Session;
   type SDP_Session_Ptr is access SDP_Session_Array;

   --  ========================================================================
   --  MICRO-SEGMENTATION
   --  ========================================================================

   --  Network segment
   type Network_Segment is record
      Segment_Name      : DNS_Records_Extended.Short_Text;
      Segment_ID        : Natural;
      CIDR_Range        : DNS_Records_Extended.Short_Text;
      VLAN_ID           : Natural range 0 .. 4095;
      Isolation_Level   : Trust_Level := Untrusted;
      Allowed_Ingress   : Access_Policy_Ptr;  --  Inbound policies
      Allowed_Egress    : Access_Policy_Ptr;  --  Outbound policies
      Default_Deny      : Boolean := True;  --  Whitelist approach
   end record;

   type Network_Segment_Array is array (Positive range <>) of Network_Segment;
   type Network_Segment_Ptr is access Network_Segment_Array;

   --  ========================================================================
   --  SDP CONTROLLER CONFIGURATION
   --  ========================================================================

   type SDP_Controller_Config is record
      Enabled               : Boolean := True;
      SPA                   : SPA_Config;
      Policies              : Access_Policy_Ptr;
      Segments              : Network_Segment_Ptr;
      Posture_Requirements  : Posture_Requirements;
      Session_Timeout       : Duration := 3600.0;
      Reauth_Interval       : Duration := 300.0;  --  Re-authenticate every 5 min
      Default_Deny          : Boolean := True;
      Log_All_Attempts      : Boolean := True;
      Integration_Backend   : Firewall_Manager.Firewall_Backend := Firewall_Manager.Firewalld;
   end record;

   --  ========================================================================
   --  SDP OPERATIONS
   --  ========================================================================

   --  Initialize SDP controller
   procedure Initialize (Config : SDP_Controller_Config);

   --  Process SPA packet (decrypt, validate, authenticate)
   function Process_SPA_Packet (
      Packet_Data : String;
      Source_IP   : String
   ) return Boolean;

   --  Authenticate user and device
   function Authenticate (
      Username  : String;
      Device_ID : String;
      MFA_Token : String := ""
   ) return Boolean;

   --  Validate device posture
   function Validate_Device_Posture (
      Device : Device_Identity
   ) return Boolean;

   --  Create SDP session (opens firewall)
   function Create_Session (
      User        : User_Identity;
      Device      : Device_Identity;
      Source_IP   : String;
      Destination : String;
      Ports       : Firewall_Manager.Port_Array_Ptr
   ) return SDP_Session;

   --  Terminate SDP session (closes firewall)
   procedure Terminate_Session (Session_ID : String);

   --  Continuous verification (periodic re-check)
   procedure Verify_Active_Sessions;

   --  Policy evaluation
   function Evaluate_Policy (
      User      : User_Identity;
      Device    : Device_Identity;
      Source_IP : String;
      Resource  : String
   ) return Access_Policy;

   --  Micro-segmentation enforcement
   procedure Enforce_Segmentation (
      Source_Segment : Network_Segment;
      Dest_Segment   : Network_Segment;
      User           : User_Identity
   );

   --  ========================================================================
   --  INTEGRATION WITH DNS/FIREWALL
   --  ========================================================================

   --  Sync SDP state with DNS (for service discovery)
   procedure Sync_With_DNS (
      Service_Name : String;
      Available    : Boolean
   );

   --  Apply SDP session as firewall rules
   procedure Apply_Firewall_Rules (Session : SDP_Session);

   --  Remove SDP session firewall rules
   procedure Remove_Firewall_Rules (Session : SDP_Session);

   --  ========================================================================
   --  MONITORING AND LOGGING
   --  ========================================================================

   --  Get SDP controller status
   function Get_Controller_Status return String;

   --  Get active sessions
   function Get_Active_Sessions return SDP_Session_Ptr;

   --  Get security events
   type Security_Event is record
      Timestamp   : Ada.Calendar.Time;
      Event_Type  : DNS_Records_Extended.Short_Text;  --  "auth_success", "auth_fail", etc.
      Username    : DNS_Records_Extended.Short_Text;
      Device_ID   : DNS_Records_Extended.Medium_Text;
      Source_IP   : DNS_Records_Extended.Short_Text;
      Details     : DNS_Records_Extended.Very_Long_Text;
      Severity    : Natural range 1 .. 10;  --  1=info, 10=critical
   end record;

   --  Log security event
   procedure Log_Security_Event (Event : Security_Event);

   --  Exceptions
   SDP_Authentication_Error : exception;
   SDP_Policy_Error : exception;
   SDP_Session_Error : exception;

end SDP_Controller;
