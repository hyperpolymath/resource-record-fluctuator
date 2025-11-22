--  Security Headers Package Body
--  Implementation of HTTP security header generation and obfuscation

with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with DNS_Records_Extended;

package body Security_Headers is

   use Ada.Strings;
   use DNS_Records_Extended;

   --  Random number generator for obfuscation
   subtype Random_Range is Natural range 1 .. 100;
   package Random_Gen is new Ada.Numerics.Discrete_Random (Random_Range);
   Gen : Random_Gen.Generator;

   --  ========================================================================
   --  FAKE SERVER/POWERED-BY POOLS
   --  ========================================================================

   Fake_Servers : constant array (1 .. 10) of String (1 .. 30) := (
      "Apache/2.4.54 (Ubuntu)      ",
      "nginx/1.18.0 (Ubuntu)       ",
      "Apache/2.4.41 (Win64)       ",
      "Microsoft-IIS/10.0          ",
      "LiteSpeed                   ",
      "Apache/2.2.34 (Unix)        ",
      "nginx/1.21.6                ",
      "Caddy                       ",
      "lighttpd/1.4.59             ",
      "Apache/2.4.52 (Debian)      "
   );

   Fake_Powered_By : constant array (1 .. 8) of String (1 .. 25) := (
      "PHP/7.4.3                ",
      "PHP/8.0.2                ",
      "ASP.NET                  ",
      "Express                  ",
      "PHP/5.6.40               ",
      "Ruby/2.7.0               ",
      "Python/3.8.10            ",
      "Perl/5.30.0              "
   );

   --  ========================================================================
   --  HEADER GENERATION
   --  ========================================================================

   function Generate_Headers (
      Config     : Security_Header_Set;
      Client_IP  : String;
      Request_URI : String := ""
   ) return String is
      use Ada.Strings.Unbounded;
      Headers : Unbounded_String;
      Is_Diagnostic : constant Boolean :=
         Is_Diagnostic_Authorized (Client_IP, Config.Server_Header.Diagnostic_IPs);
   begin
      --  Server header
      case Config.Server_Header.Mode is
         when Hidden =>
            null;  --  Don't send Server header

         when Standard =>
            Append (Headers, "Server: " & To_String (Config.Server_Header.Real_Value) & ASCII.LF);

         when Obfuscated =>
            Append (Headers, "Server: " & To_String (Config.Server_Header.Fake_Value) & ASCII.LF);

         when Diagnostic =>
            if Is_Diagnostic then
               Append (Headers, "Server: " & To_String (Config.Server_Header.Real_Value) & ASCII.LF);
            else
               Append (Headers, "Server: " & To_String (Config.Server_Header.Fake_Value) & ASCII.LF);
            end if;
      end case;

      --  X-Powered-By header
      case Config.Powered_By_Header.Mode is
         when Hidden =>
            null;

         when Standard =>
            Append (Headers, "X-Powered-By: " &
                    To_String (Config.Powered_By_Header.Real_Value) & ASCII.LF);

         when Obfuscated =>
            Append (Headers, "X-Powered-By: " &
                    To_String (Config.Powered_By_Header.Fake_Value) & ASCII.LF);

         when Diagnostic =>
            if Is_Diagnostic then
               Append (Headers, "X-Powered-By: " &
                       To_String (Config.Powered_By_Header.Real_Value) & ASCII.LF);
            else
               Append (Headers, "X-Powered-By: " &
                       To_String (Config.Powered_By_Header.Fake_Value) & ASCII.LF);
            end if;
      end case;

      --  HSTS (Strict-Transport-Security)
      if Config.HSTS.Enabled then
         Append (Headers, "Strict-Transport-Security: max-age=" &
                 Trim (Config.HSTS.Max_Age'Img, Left));
         if Config.HSTS.Include_Subdomains then
            Append (Headers, "; includeSubDomains");
         end if;
         if Config.HSTS.Preload then
            Append (Headers, "; preload");
         end if;
         Append (Headers, ASCII.LF);
      end if;

      --  Content-Security-Policy
      if Config.CSP.Enabled then
         if Config.CSP.Report_Only then
            Append (Headers, "Content-Security-Policy-Report-Only: ");
         else
            Append (Headers, "Content-Security-Policy: ");
         end if;

         --  Add directives
         for I in Config.CSP.Directives.First_Index .. Config.CSP.Directives.Last_Index loop
            if I > Config.CSP.Directives.First_Index then
               Append (Headers, "; ");
            end if;
            Append (Headers, To_String (Config.CSP.Directives (I)));
         end loop;

         Append (Headers, ASCII.LF);
      end if;

      --  X-Frame-Options
      case Config.X_Frame_Options is
         when Deny =>
            Append (Headers, "X-Frame-Options: DENY" & ASCII.LF);
         when SameOrigin =>
            Append (Headers, "X-Frame-Options: SAMEORIGIN" & ASCII.LF);
         when Allow_From =>
            Append (Headers, "X-Frame-Options: ALLOW-FROM" & ASCII.LF);
      end case;

      --  X-Content-Type-Options
      if Config.X_Content_Type_Options = NoSniff then
         Append (Headers, "X-Content-Type-Options: nosniff" & ASCII.LF);
      end if;

      --  Referrer-Policy
      case Config.Referrer_Policy is
         when No_Referrer =>
            Append (Headers, "Referrer-Policy: no-referrer" & ASCII.LF);
         when No_Referrer_When_Downgrade =>
            Append (Headers, "Referrer-Policy: no-referrer-when-downgrade" & ASCII.LF);
         when Origin =>
            Append (Headers, "Referrer-Policy: origin" & ASCII.LF);
         when Origin_When_Cross_Origin =>
            Append (Headers, "Referrer-Policy: origin-when-cross-origin" & ASCII.LF);
         when Same_Origin =>
            Append (Headers, "Referrer-Policy: same-origin" & ASCII.LF);
         when Strict_Origin =>
            Append (Headers, "Referrer-Policy: strict-origin" & ASCII.LF);
         when Strict_Origin_When_Cross_Origin =>
            Append (Headers, "Referrer-Policy: strict-origin-when-cross-origin" & ASCII.LF);
         when Unsafe_URL =>
            Append (Headers, "Referrer-Policy: unsafe-url" & ASCII.LF);
      end case;

      --  Permissions-Policy
      if Config.Permissions_Policy.Enabled then
         Append (Headers, "Permissions-Policy: " &
                 To_String (Config.Permissions_Policy.Features) & ASCII.LF);
      end if;

      --  COEP (Cross-Origin-Embedder-Policy)
      case Config.COEP is
         when Unsafe_None =>
            Append (Headers, "Cross-Origin-Embedder-Policy: unsafe-none" & ASCII.LF);
         when Require_Corp =>
            Append (Headers, "Cross-Origin-Embedder-Policy: require-corp" & ASCII.LF);
         when Credentialless =>
            Append (Headers, "Cross-Origin-Embedder-Policy: credentialless" & ASCII.LF);
      end case;

      --  COOP (Cross-Origin-Opener-Policy)
      case Config.COOP is
         when Unsafe_None =>
            Append (Headers, "Cross-Origin-Opener-Policy: unsafe-none" & ASCII.LF);
         when Same_Origin_Allow_Popups =>
            Append (Headers, "Cross-Origin-Opener-Policy: same-origin-allow-popups" & ASCII.LF);
         when Same_Origin =>
            Append (Headers, "Cross-Origin-Opener-Policy: same-origin" & ASCII.LF);
         when Same_Origin_Plus_COEP =>
            Append (Headers, "Cross-Origin-Opener-Policy: same-origin-plus-coep" & ASCII.LF);
      end case;

      --  CORP (Cross-Origin-Resource-Policy)
      case Config.CORP is
         when Same_Site =>
            Append (Headers, "Cross-Origin-Resource-Policy: same-site" & ASCII.LF);
         when Same_Origin =>
            Append (Headers, "Cross-Origin-Resource-Policy: same-origin" & ASCII.LF);
         when Cross_Origin =>
            Append (Headers, "Cross-Origin-Resource-Policy: cross-origin" & ASCII.LF);
      end case;

      --  Custom headers
      if Length (Config.Custom_Headers) > 0 then
         Append (Headers, To_String (Config.Custom_Headers) & ASCII.LF);
      end if;

      return To_String (Headers);
   end Generate_Headers;

   function Generate_CSP_Nonce return String is
      use Ada.Calendar;
      Now : constant Time := Clock;
      Nonce_Seed : constant Natural := Natural (Seconds (Now) * 1000.0);
      Nonce_Val : Natural := Nonce_Seed;
      Hex_Digits : constant String := "0123456789abcdef";
      Result : String (1 .. 32);
   begin
      --  Generate a simple pseudo-random nonce
      for I in Result'Range loop
         Nonce_Val := (Nonce_Val * 1103515245 + 12345) mod 2147483647;
         Result (I) := Hex_Digits ((Nonce_Val mod 16) + 1);
      end loop;

      return Result;
   end Generate_CSP_Nonce;

   function Is_Diagnostic_Authorized (
      IP      : String;
      Allowed : DNS_Records_Extended.APL_Entry_Array_Ptr
   ) return Boolean is
   begin
      if Allowed = null then
         return False;
      end if;

      --  Check if IP matches any allowed entry
      for Entry of Allowed.all loop
         --  Simplified check - full implementation would use APL_Matches
         --  For now, just check for exact match or common patterns
         if IP = "127.0.0.1" or IP = "::1" then
            return True;  --  Localhost always allowed for diagnostics
         end if;
      end loop;

      return False;
   end Is_Diagnostic_Authorized;

   procedure Randomize_Obfuscated_Headers (
      Config : in out Security_Header_Set
   ) is
   begin
      if Config.Server_Header.Randomize then
         Config.Server_Header.Fake_Value :=
            To_Bounded_String (Get_Random_Server_Header);
      end if;

      if Config.Powered_By_Header.Randomize then
         Config.Powered_By_Header.Fake_Value :=
            To_Bounded_String (Get_Random_Powered_By_Header);
      end if;
   end Randomize_Obfuscated_Headers;

   function Get_Random_Server_Header return String is
      Idx : constant Positive := Random_Gen.Random (Gen) mod Fake_Servers'Length + 1;
   begin
      return Trim (Fake_Servers (Idx), Both);
   end Get_Random_Server_Header;

   function Get_Random_Powered_By_Header return String is
      Idx : constant Positive := Random_Gen.Random (Gen) mod Fake_Powered_By'Length + 1;
   begin
      return Trim (Fake_Powered_By (Idx), Both);
   end Get_Random_Powered_By_Header;

   --  ========================================================================
   --  WEB SERVER CONFIGURATION GENERATION
   --  ========================================================================

   function Generate_Nginx_Config (
      Config : Security_Header_Set
   ) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "# Nginx Security Headers Configuration" & ASCII.LF);
      Append (Result, "# Generated by HINFO-LOC Fluctuator" & ASCII.LF);
      Append (Result, ASCII.LF);

      --  Server header obfuscation
      case Config.Server_Header.Mode is
         when Hidden =>
            Append (Result, "server_tokens off;" & ASCII.LF);
            Append (Result, "more_clear_headers Server;" & ASCII.LF);

         when Obfuscated | Diagnostic =>
            Append (Result, "server_tokens off;" & ASCII.LF);
            Append (Result, "more_set_headers 'Server: " &
                    To_String (Config.Server_Header.Fake_Value) & "';" & ASCII.LF);

         when Standard =>
            null;  --  Use default
      end case;

      --  HSTS
      if Config.HSTS.Enabled then
         Append (Result, "add_header Strict-Transport-Security ""max-age=" &
                 Trim (Config.HSTS.Max_Age'Img, Left));
         if Config.HSTS.Include_Subdomains then
            Append (Result, "; includeSubDomains");
         end if;
         if Config.HSTS.Preload then
            Append (Result, "; preload");
         end if;
         Append (Result, """ always;" & ASCII.LF);
      end if;

      --  X-Frame-Options
      case Config.X_Frame_Options is
         when Deny =>
            Append (Result, "add_header X-Frame-Options ""DENY"" always;" & ASCII.LF);
         when SameOrigin =>
            Append (Result, "add_header X-Frame-Options ""SAMEORIGIN"" always;" & ASCII.LF);
         when Allow_From =>
            null;
      end case;

      --  X-Content-Type-Options
      if Config.X_Content_Type_Options = NoSniff then
         Append (Result, "add_header X-Content-Type-Options ""nosniff"" always;" & ASCII.LF);
      end if;

      return To_String (Result);
   end Generate_Nginx_Config;

   function Generate_Apache_Config (
      Config : Security_Header_Set
   ) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "# Apache Security Headers Configuration" & ASCII.LF);
      Append (Result, "# Generated by HINFO-LOC Fluctuator" & ASCII.LF);
      Append (Result, ASCII.LF);

      --  Server header
      case Config.Server_Header.Mode is
         when Hidden =>
            Append (Result, "ServerTokens Prod" & ASCII.LF);
            Append (Result, "ServerSignature Off" & ASCII.LF);
            Append (Result, "Header unset Server" & ASCII.LF);

         when Obfuscated | Diagnostic =>
            Append (Result, "ServerTokens Prod" & ASCII.LF);
            Append (Result, "Header set Server """ &
                    To_String (Config.Server_Header.Fake_Value) & """" & ASCII.LF);

         when Standard =>
            null;
      end case;

      --  HSTS
      if Config.HSTS.Enabled then
         Append (Result, "Header always set Strict-Transport-Security ""max-age=" &
                 Trim (Config.HSTS.Max_Age'Img, Left));
         if Config.HSTS.Include_Subdomains then
            Append (Result, "; includeSubDomains");
         end if;
         if Config.HSTS.Preload then
            Append (Result, "; preload");
         end if;
         Append (Result, """" & ASCII.LF);
      end if;

      return To_String (Result);
   end Generate_Apache_Config;

   function Generate_PHP_Config (
      Config : Tech_Stack_Config
   ) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "; PHP Configuration for Security" & ASCII.LF);
      Append (Result, "; Generated by HINFO-LOC Fluctuator" & ASCII.LF);
      Append (Result, ASCII.LF);

      if not Config.Expose_PHP_Version then
         Append (Result, "expose_php = Off" & ASCII.LF);
      end if;

      return To_String (Result);
   end Generate_PHP_Config;

   --  ========================================================================
   --  DIAGNOSTIC MODE
   --  ========================================================================

   function Get_Diagnostic_Info (
      Token : String;
      Config : Security_Header_Set
   ) return Diagnostic_Info is
      Info : Diagnostic_Info;
   begin
      --  Verify token
      if To_String (Config.Tech_Stack.Diagnostic_Token) /= Token then
         raise Constraint_Error with "Invalid diagnostic token";
      end if;

      --  Populate diagnostic information
      Info.Server_Software := Config.Server_Header.Real_Value;
      Info.PHP_Version := Config.Powered_By_Header.Real_Value;
      Info.Current_Time := Ada.Calendar.Clock;

      return Info;
   end Get_Diagnostic_Info;

   function Generate_Diagnostic_Response (
      Info   : Diagnostic_Info;
      Format : String := "json"
   ) return String is
      use Ada.Strings.Unbounded;
      use Ada.Calendar.Formatting;
      Result : Unbounded_String;
   begin
      if Format = "json" then
         Append (Result, "{" & ASCII.LF);
         Append (Result, "  ""server_software"": """ & To_String (Info.Server_Software) & """," & ASCII.LF);
         Append (Result, "  ""php_version"": """ & To_String (Info.PHP_Version) & """," & ASCII.LF);
         Append (Result, "  ""framework"": """ & To_String (Info.Framework) & """," & ASCII.LF);
         Append (Result, "  ""database"": """ & To_String (Info.Database) & """," & ASCII.LF);
         Append (Result, "  ""current_time"": """ & Image (Info.Current_Time) & """" & ASCII.LF);
         Append (Result, "}" & ASCII.LF);
      else
         --  HTML format
         Append (Result, "<html><head><title>Diagnostic Info</title></head><body>" & ASCII.LF);
         Append (Result, "<h1>System Diagnostics</h1>" & ASCII.LF);
         Append (Result, "<table border='1'>" & ASCII.LF);
         Append (Result, "<tr><th>Item</th><th>Value</th></tr>" & ASCII.LF);
         Append (Result, "<tr><td>Server Software</td><td>" &
                 To_String (Info.Server_Software) & "</td></tr>" & ASCII.LF);
         Append (Result, "<tr><td>PHP Version</td><td>" &
                 To_String (Info.PHP_Version) & "</td></tr>" & ASCII.LF);
         Append (Result, "<tr><td>Framework</td><td>" &
                 To_String (Info.Framework) & "</td></tr>" & ASCII.LF);
         Append (Result, "<tr><td>Database</td><td>" &
                 To_String (Info.Database) & "</td></tr>" & ASCII.LF);
         Append (Result, "<tr><td>Current Time</td><td>" &
                 Image (Info.Current_Time) & "</td></tr>" & ASCII.LF);
         Append (Result, "</table></body></html>" & ASCII.LF);
      end if;

      return To_String (Result);
   end Generate_Diagnostic_Response;

   --  ========================================================================
   --  HINFO INTEGRATION
   --  ========================================================================

   procedure Sync_With_HINFO (
      Config     : in out Security_Header_Set;
      HINFO_CPU  : String;
      HINFO_OS   : String
   ) is
   begin
      --  Update fake server headers to match HINFO obfuscation
      --  This ensures consistency between DNS and HTTP headers

      --  Map common CPU types to server software
      if Index (HINFO_CPU, "INTEL") > 0 or Index (HINFO_CPU, "AMD") > 0 then
         Config.Server_Header.Fake_Value := To_Bounded_String ("Apache/2.4.54 (Ubuntu)");
      elsif Index (HINFO_CPU, "ARM") > 0 then
         Config.Server_Header.Fake_Value := To_Bounded_String ("nginx/1.21.6");
      end if;

      --  Map OS to Powered-By
      if Index (HINFO_OS, "LINUX") > 0 then
         Config.Powered_By_Header.Fake_Value := To_Bounded_String ("PHP/8.0.2");
      elsif Index (HINFO_OS, "WINDOWS") > 0 then
         Config.Powered_By_Header.Fake_Value := To_Bounded_String ("ASP.NET");
      end if;
   end Sync_With_HINFO;

   procedure Generate_Consistent_Fake_Stack (
      Config : in out Security_Header_Set;
      HINFO_CPU  : out String;
      HINFO_OS   : out String
   ) is
   begin
      --  Generate consistent fake technology stack
      --  Choose a random combination and set both HINFO and headers

      declare
         Stack_Choice : constant Natural := Random_Gen.Random (Gen) mod 4;
      begin
         case Stack_Choice is
            when 0 =>
               --  Linux/Apache/PHP stack
               HINFO_CPU := "INTEL-X86_64";
               HINFO_OS := "LINUX";
               Config.Server_Header.Fake_Value := To_Bounded_String ("Apache/2.4.54 (Ubuntu)");
               Config.Powered_By_Header.Fake_Value := To_Bounded_String ("PHP/8.0.2");

            when 1 =>
               --  Windows/IIS/.NET stack
               HINFO_CPU := "AMD64";
               HINFO_OS := "WINDOWS-SERVER-2019";
               Config.Server_Header.Fake_Value := To_Bounded_String ("Microsoft-IIS/10.0");
               Config.Powered_By_Header.Fake_Value := To_Bounded_String ("ASP.NET");

            when 2 =>
               --  nginx/Node.js stack
               HINFO_CPU := "ARM64";
               HINFO_OS := "DEBIAN";
               Config.Server_Header.Fake_Value := To_Bounded_String ("nginx/1.21.6");
               Config.Powered_By_Header.Fake_Value := To_Bounded_String ("Express");

            when others =>
               --  BSD/nginx stack
               HINFO_CPU := "X86_64";
               HINFO_OS := "FREEBSD";
               Config.Server_Header.Fake_Value := To_Bounded_String ("nginx/1.18.0");
               Config.Powered_By_Header.Fake_Value := To_Bounded_String ("Ruby/2.7.0");
         end case;
      end;
   end Generate_Consistent_Fake_Stack;

begin
   --  Initialize random number generator
   Random_Gen.Reset (Gen);
end Security_Headers;
