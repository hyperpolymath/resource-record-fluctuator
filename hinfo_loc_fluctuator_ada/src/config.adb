--  Configuration File Parser Body
--  INI-style configuration implementation

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Config is

   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;

   --  Get default configuration values
   function Default_Config return Application_Config is
      C : Application_Config;
   begin
      C.CPU_Data_File := String_256.To_Bounded_String ("data/machines.txt");
      C.OS_Data_File := String_256.To_Bounded_String ("data/operating_systems.txt");
      C.Location_Data_File := String_256.To_Bounded_String ("data/locations.csv");

      C.Log_File := String_256.To_Bounded_String ("hinfo_loc_fluctuator.log");
      C.Log_Level := String_256.To_Bounded_String ("info");
      C.Log_Console := True;

      C.Session_Timeout := 1800.0;
      C.Max_Login_Attempts := 3;

      C.DNS_Server := String_256.To_Bounded_String ("127.0.0.1");
      C.DNS_Port := 53;
      C.TSIG_Key_File := String_256.To_Bounded_String ("");

      C.Auto_Fluctuate := False;
      C.Fluctuation_Interval := 3600.0;
      C.Default_TTL := 300;

      C.Zone_File_Path := String_256.To_Bounded_String ("zone.db");
      C.Zone_Origin := String_256.To_Bounded_String ("example.com");
      C.Primary_NS := String_256.To_Bounded_String ("ns1.example.com");
      C.Hostmaster := String_256.To_Bounded_String ("admin.example.com");

      return C;
   end Default_Config;

   --  Parse boolean value
   function Parse_Boolean (Value : String) return Boolean is
      Lower : constant String := Translate (Value, Maps.Lower_Case_Map);
   begin
      if Lower = "true" or Lower = "yes" or Lower = "1" or Lower = "on" then
         return True;
      elsif Lower = "false" or Lower = "no" or Lower = "0" or Lower = "off" then
         return False;
      else
         raise Config_Error with "Invalid boolean value: " & Value;
      end if;
   end Parse_Boolean;

   --  Parse key=value line and update config
   procedure Parse_Line (
      Line   : String;
      Section : in out String_256.Bounded_String;
      Config : in out Application_Config
   ) is
      Trimmed : constant String := Trim (Line, Both);
      Equals_Pos : Natural;
   begin
      --  Skip empty lines and comments
      if Trimmed'Length = 0 or else Trimmed (Trimmed'First) = '#' then
         return;
      end if;

      --  Check for section header [section]
      if Trimmed (Trimmed'First) = '[' and Trimmed (Trimmed'Last) = ']' then
         Section := String_256.To_Bounded_String (
            Trimmed (Trimmed'First + 1 .. Trimmed'Last - 1)
         );
         return;
      end if;

      --  Parse key=value
      Equals_Pos := Index (Trimmed, "=");
      if Equals_Pos = 0 then
         return;  --  Not a valid key=value line
      end if;

      declare
         Key   : constant String := Trim (Trimmed (Trimmed'First .. Equals_Pos - 1), Both);
         Value : constant String := Trim (Trimmed (Equals_Pos + 1 .. Trimmed'Last), Both);
         Sect  : constant String := String_256.To_String (Section);
      begin
         --  Parse based on section and key
         if Sect = "data" then
            if Key = "cpu_file" then
               Config.CPU_Data_File := String_256.To_Bounded_String (Value);
            elsif Key = "os_file" then
               Config.OS_Data_File := String_256.To_Bounded_String (Value);
            elsif Key = "location_file" then
               Config.Location_Data_File := String_256.To_Bounded_String (Value);
            end if;

         elsif Sect = "logging" then
            if Key = "file" then
               Config.Log_File := String_256.To_Bounded_String (Value);
            elsif Key = "level" then
               Config.Log_Level := String_256.To_Bounded_String (Value);
            elsif Key = "console" then
               Config.Log_Console := Parse_Boolean (Value);
            end if;

         elsif Sect = "security" then
            if Key = "session_timeout" then
               Config.Session_Timeout := Duration'Value (Value);
            elsif Key = "max_login_attempts" then
               Config.Max_Login_Attempts := Natural'Value (Value);
            end if;

         elsif Sect = "dns" then
            if Key = "server" then
               Config.DNS_Server := String_256.To_Bounded_String (Value);
            elsif Key = "port" then
               Config.DNS_Port := Natural'Value (Value);
            elsif Key = "tsig_key_file" then
               Config.TSIG_Key_File := String_256.To_Bounded_String (Value);
            end if;

         elsif Sect = "fluctuation" then
            if Key = "auto" then
               Config.Auto_Fluctuate := Parse_Boolean (Value);
            elsif Key = "interval" then
               Config.Fluctuation_Interval := Duration'Value (Value);
            elsif Key = "default_ttl" then
               Config.Default_TTL := DNS_Records.TTL_Seconds'Value (Value);
            end if;

         elsif Sect = "zone" then
            if Key = "file" then
               Config.Zone_File_Path := String_256.To_Bounded_String (Value);
            elsif Key = "origin" then
               Config.Zone_Origin := String_256.To_Bounded_String (Value);
            elsif Key = "primary_ns" then
               Config.Primary_NS := String_256.To_Bounded_String (Value);
            elsif Key = "hostmaster" then
               Config.Hostmaster := String_256.To_Bounded_String (Value);
            end if;
         end if;
      end;
   end Parse_Line;

   --  Load configuration from INI file
   procedure Load_Config (
      Filename : String;
      Config   : out Application_Config
   ) is
      File    : File_Type;
      Section : String_256.Bounded_String := String_256.To_Bounded_String ("");
   begin
      --  Start with defaults
      Config := Default_Config;

      --  Try to open file
      begin
         Open (File, In_File, Filename);
      exception
         when Name_Error =>
            raise Config_Error with "Configuration file not found: " & Filename;
      end;

      --  Parse each line
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Parse_Line (Line, Section, Config);
         end;
      end loop;

      Close (File);

   exception
      when E : others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_Config;

   --  Save configuration to INI file
   procedure Save_Config (
      Filename : String;
      Config   : Application_Config
   ) is
      File : File_Type;

      procedure Put_Config (Section, Key, Value : String) is
      begin
         if Section'Length > 0 then
            Put_Line (File, "");
            Put_Line (File, "[" & Section & "]");
         end if;
         Put_Line (File, Key & "=" & Value);
      end Put_Config;

      function Bool_To_String (B : Boolean) return String is
      begin
         return (if B then "true" else "false");
      end Bool_To_String;

   begin
      Create (File, Out_File, Filename);

      Put_Line (File, "# HINFO-LOC Fluctuator Configuration");
      Put_Line (File, "# Generated configuration file");
      Put_Line (File, "");

      Put_Line (File, "[data]");
      Put_Line (File, "cpu_file=" & String_256.To_String (Config.CPU_Data_File));
      Put_Line (File, "os_file=" & String_256.To_String (Config.OS_Data_File));
      Put_Line (File, "location_file=" & String_256.To_String (Config.Location_Data_File));

      Put_Line (File, "");
      Put_Line (File, "[logging]");
      Put_Line (File, "file=" & String_256.To_String (Config.Log_File));
      Put_Line (File, "level=" & String_256.To_String (Config.Log_Level));
      Put_Line (File, "console=" & Bool_To_String (Config.Log_Console));

      Put_Line (File, "");
      Put_Line (File, "[security]");
      Put_Line (File, "session_timeout=" & Duration'Image (Config.Session_Timeout));
      Put_Line (File, "max_login_attempts=" & Natural'Image (Config.Max_Login_Attempts));

      Put_Line (File, "");
      Put_Line (File, "[dns]");
      Put_Line (File, "server=" & String_256.To_String (Config.DNS_Server));
      Put_Line (File, "port=" & Natural'Image (Config.DNS_Port));
      Put_Line (File, "tsig_key_file=" & String_256.To_String (Config.TSIG_Key_File));

      Put_Line (File, "");
      Put_Line (File, "[fluctuation]");
      Put_Line (File, "auto=" & Bool_To_String (Config.Auto_Fluctuate));
      Put_Line (File, "interval=" & Duration'Image (Config.Fluctuation_Interval));
      Put_Line (File, "default_ttl=" & DNS_Records.TTL_Seconds'Image (Config.Default_TTL));

      Put_Line (File, "");
      Put_Line (File, "[zone]");
      Put_Line (File, "file=" & String_256.To_String (Config.Zone_File_Path));
      Put_Line (File, "origin=" & String_256.To_String (Config.Zone_Origin));
      Put_Line (File, "primary_ns=" & String_256.To_String (Config.Primary_NS));
      Put_Line (File, "hostmaster=" & String_256.To_String (Config.Hostmaster));

      Close (File);
   end Save_Config;

end Config;
