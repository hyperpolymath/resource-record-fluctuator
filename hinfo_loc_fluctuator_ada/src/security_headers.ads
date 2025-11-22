--  Security Headers Management Specification
--  Control HTTP security headers and server obfuscation
--
--  Features:
--  - All standard and experimental security headers
--  - Server header obfuscation
--  - X-Powered-By hiding
--  - Diagnostic mode (expose stack only to maintainers)
--  - Per-IP header policies
--  - Integration with HINFO fluctuation

with DNS_Records_Extended;
with Ada.Containers.Vectors;

package Security_Headers is

   --  ========================================================================
   --  HEADER EXPOSURE MODES
   --  ========================================================================

   type Exposure_Mode is (
      Hidden,           --  Don't send header at all
      Obfuscated,       --  Send fake/random value
      Diagnostic,       --  Show real value to authorized IPs only
      Standard          --  Show real value to everyone
   );

   --  ========================================================================
   --  SERVER IDENTIFICATION
   --  ========================================================================

   --  Server header configuration
   type Server_Header_Config is record
      Mode           : Exposure_Mode := Obfuscated;
      Real_Value     : DNS_Records_Extended.Short_Text;  --  e.g., "nginx/1.21.0"
      Fake_Value     : DNS_Records_Extended.Short_Text;  --  e.g., "Apache/2.4.41"
      Diagnostic_IPs : DNS_Records_Extended.APL_Entry_Array_Ptr;  --  Show real to these
      Randomize      : Boolean := True;  --  Change fake value periodically
   end record;

   --  X-Powered-By header configuration
   type Powered_By_Config is record
      Mode           : Exposure_Mode := Hidden;
      Real_Value     : DNS_Records_Extended.Short_Text;  --  e.g., "PHP/8.1.2"
      Fake_Value     : DNS_Records_Extended.Short_Text;
      Diagnostic_IPs : DNS_Records_Extended.APL_Entry_Array_Ptr;
      Randomize      : Boolean := True;
   end record;

   --  Technology fingerprint obfuscation
   type Tech_Stack_Config is record
      Expose_PHP_Version     : Boolean := False;
      Expose_Framework       : Boolean := False;  --  Laravel, React, etc.
      Expose_Database        : Boolean := False;
      Diagnostic_Mode        : Boolean := False;  --  Allow diagnostics
      Diagnostic_Token       : DNS_Records_Extended.Medium_Text;  --  Secret token
      Diagnostic_IPs         : DNS_Records_Extended.APL_Entry_Array_Ptr;
   end record;

   --  ========================================================================
   --  STANDARD SECURITY HEADERS
   --  ========================================================================

   --  Strict-Transport-Security (HSTS)
   type HSTS_Config is record
      Enabled            : Boolean := True;
      Max_Age            : Natural := 31536000;  --  1 year in seconds
      Include_Subdomains : Boolean := True;
      Preload            : Boolean := False;  --  Submit to HSTS preload list
   end record;

   --  Content-Security-Policy (CSP)
   type CSP_Directive_Type is (
      Default_Src,
      Script_Src,
      Style_Src,
      Img_Src,
      Font_Src,
      Connect_Src,
      Media_Src,
      Object_Src,
      Frame_Src,
      Worker_Src,
      Manifest_Src,
      Base_Uri,
      Form_Action,
      Frame_Ancestors,
      Upgrade_Insecure_Requests,
      Block_All_Mixed_Content,
      Report_Uri,
      Report_To
   );

   package CSP_Directive_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => DNS_Records_Extended.Medium_Text,
      "="          => DNS_Records_Extended.String_512."="
   );

   type CSP_Config is record
      Enabled            : Boolean := True;
      Report_Only        : Boolean := False;  --  Test mode
      Directives         : CSP_Directive_Vectors.Vector;
      Nonce_Enabled      : Boolean := True;  --  Use nonces for inline scripts
      Strict_Dynamic     : Boolean := False;  --  CSP Level 3
   end record;

   --  X-Frame-Options
   type Frame_Options is (
      Deny,
      SameOrigin,
      Allow_From  --  Deprecated, use CSP frame-ancestors
   );

   --  X-Content-Type-Options
   type Content_Type_Options is (
      NoSniff,
      Disabled
   );

   --  Referrer-Policy
   type Referrer_Policy_Type is (
      No_Referrer,
      No_Referrer_When_Downgrade,
      Origin,
      Origin_When_Cross_Origin,
      Same_Origin,
      Strict_Origin,
      Strict_Origin_When_Cross_Origin,
      Unsafe_URL
   );

   --  Permissions-Policy (formerly Feature-Policy)
   type Permission_Policy_Feature is (
      Accelerometer,
      Ambient_Light_Sensor,
      Autoplay,
      Battery,
      Camera,
      Display_Capture,
      Document_Domain,
      Encrypted_Media,
      Fullscreen,
      Geolocation,
      Gyroscope,
      Magnetometer,
      Microphone,
      Midi,
      Payment,
      Picture_In_Picture,
      Publickey_Credentials_Get,
      Screen_Wake_Lock,
      Sync_XHR,
      USB,
      Web_Share,
      XR_Spatial_Tracking
   );

   type Permission_Policy_Config is record
      Enabled  : Boolean := True;
      Features : DNS_Records_Extended.Very_Long_Text;  --  Formatted policy string
   end record;

   --  ========================================================================
   --  EXPERIMENTAL SECURITY HEADERS
   --  ========================================================================

   --  Cross-Origin-Embedder-Policy (COEP)
   type COEP_Type is (
      Unsafe_None,
      Require_Corp,
      Credentialless
   );

   --  Cross-Origin-Opener-Policy (COOP)
   type COOP_Type is (
      Unsafe_None,
      Same_Origin_Allow_Popups,
      Same_Origin,
      Same_Origin_Plus_COEP
   );

   --  Cross-Origin-Resource-Policy (CORP)
   type CORP_Type is (
      Same_Site,
      Same_Origin,
      Cross_Origin
   );

   --  Expect-CT (Certificate Transparency)
   type Expect_CT_Config is record
      Enabled    : Boolean := False;  --  Being deprecated
      Max_Age    : Natural := 86400;
      Enforce    : Boolean := False;
      Report_Uri : DNS_Records_Extended.Medium_Text;
   end record;

   --  NEL (Network Error Logging)
   type NEL_Config is record
      Enabled     : Boolean := False;
      Report_To   : DNS_Records_Extended.Short_Text;
      Max_Age     : Natural := 2592000;  --  30 days
      Include_Subdomains : Boolean := False;
   end record;

   --  ========================================================================
   --  COMPREHENSIVE SECURITY HEADER SET
   --  ========================================================================

   type Security_Header_Set is record
      --  Server identification
      Server_Header         : Server_Header_Config;
      Powered_By_Header     : Powered_By_Config;
      Tech_Stack            : Tech_Stack_Config;

      --  Standard security headers
      HSTS                  : HSTS_Config;
      CSP                   : CSP_Config;
      X_Frame_Options       : Frame_Options := Deny;
      X_Content_Type_Options : Content_Type_Options := NoSniff;
      Referrer_Policy       : Referrer_Policy_Type := Strict_Origin_When_Cross_Origin;
      Permissions_Policy    : Permission_Policy_Config;

      --  XSS Protection (deprecated but still used)
      X_XSS_Protection      : Boolean := False;  --  Disabled, use CSP instead

      --  Experimental headers
      COEP                  : COEP_Type := Unsafe_None;
      COOP                  : COOP_Type := Unsafe_None;
      CORP                  : CORP_Type := Same_Origin;
      Expect_CT             : Expect_CT_Config;
      NEL                   : NEL_Config;

      --  Custom headers
      Custom_Headers        : DNS_Records_Extended.Very_Long_Text;
   end record;

   --  ========================================================================
   --  HEADER GENERATION AND APPLICATION
   --  ========================================================================

   --  Generate HTTP headers for response
   function Generate_Headers (
      Config     : Security_Header_Set;
      Client_IP  : String;  --  To check diagnostic access
      Request_URI : String := ""  --  For CSP nonce generation
   ) return String;

   --  Generate CSP nonce for inline scripts
   function Generate_CSP_Nonce return String;

   --  Check if IP is authorized for diagnostics
   function Is_Diagnostic_Authorized (
      IP      : String;
      Allowed : DNS_Records_Extended.APL_Entry_Array_Ptr
   ) return Boolean;

   --  Randomize obfuscated values (for periodic changes)
   procedure Randomize_Obfuscated_Headers (
      Config : in out Security_Header_Set
   );

   --  Get predefined fake server values (pool for randomization)
   function Get_Random_Server_Header return String;
   function Get_Random_Powered_By_Header return String;

   --  ========================================================================
   --  WEB SERVER INTEGRATION
   --  ========================================================================

   --  Generate nginx configuration
   function Generate_Nginx_Config (
      Config : Security_Header_Set
   ) return String;

   --  Generate Apache configuration
   function Generate_Apache_Config (
      Config : Security_Header_Set
   ) return String;

   --  Generate PHP configuration (php.ini snippet)
   function Generate_PHP_Config (
      Config : Tech_Stack_Config
   ) return String;

   --  ========================================================================
   --  DIAGNOSTIC MODE
   --  ========================================================================

   --  Diagnostic response for authorized maintainers
   type Diagnostic_Info is record
      Server_Software    : DNS_Records_Extended.Short_Text;
      PHP_Version        : DNS_Records_Extended.Short_Text;
      Framework          : DNS_Records_Extended.Short_Text;
      Database           : DNS_Records_Extended.Short_Text;
      Modules_Loaded     : DNS_Records_Extended.Very_Long_Text;
      Security_Headers   : DNS_Records_Extended.Very_Long_Text;
      Current_Time       : Ada.Calendar.Time;
   end record;

   function Get_Diagnostic_Info (
      Token : String;
      Config : Security_Header_Set
   ) return Diagnostic_Info;

   --  Generate diagnostic response (JSON or HTML)
   function Generate_Diagnostic_Response (
      Info   : Diagnostic_Info;
      Format : String := "json"  --  "json" or "html"
   ) return String;

   --  ========================================================================
   --  INTEGRATION WITH HINFO FLUCTUATION
   --  ========================================================================

   --  Sync server headers with HINFO record
   procedure Sync_With_HINFO (
      Config     : in out Security_Header_Set;
      HINFO_CPU  : String;
      HINFO_OS   : String
   );

   --  Generate consistent fake stack across HINFO and headers
   procedure Generate_Consistent_Fake_Stack (
      Config : in out Security_Header_Set;
      HINFO_CPU  : out String;
      HINFO_OS   : out String
   );

end Security_Headers;
