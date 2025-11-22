--  Configuration File Parser Specification
--  Simple INI-style configuration format
--
--  Format:
--  [section]
--  key=value
--  # comments

with Ada.Strings.Bounded;
with DNS_Records;

package Config is

   --  Configuration sections
   package String_256 is new Ada.Strings.Bounded.Generic_Bounded_Length (256);
   subtype Config_String is String_256.Bounded_String;

   --  Main configuration record
   type Application_Config is record
      --  Data file paths
      CPU_Data_File      : Config_String;
      OS_Data_File       : Config_String;
      Location_Data_File : Config_String;

      --  Logging configuration
      Log_File           : Config_String;
      Log_Level          : Config_String;  --  debug, info, warning, error, critical
      Log_Console        : Boolean := True;

      --  Security settings
      Session_Timeout    : Duration := 1800.0;  --  30 minutes
      Max_Login_Attempts : Natural := 3;

      --  DNS settings
      DNS_Server         : Config_String;
      DNS_Port           : Natural := 53;
      TSIG_Key_File      : Config_String;

      --  Fluctuation settings
      Auto_Fluctuate     : Boolean := False;
      Fluctuation_Interval : Duration := 3600.0;  --  1 hour
      Default_TTL        : DNS_Records.TTL_Seconds := 300;

      --  Zone file settings
      Zone_File_Path     : Config_String;
      Zone_Origin        : Config_String;
      Primary_NS         : Config_String;
      Hostmaster         : Config_String;
   end record;

   --  Load configuration from file
   procedure Load_Config (
      Filename : String;
      Config   : out Application_Config
   );

   --  Save configuration to file
   procedure Save_Config (
      Filename : String;
      Config   : Application_Config
   );

   --  Get default configuration
   function Default_Config return Application_Config;

   --  Configuration parsing exception
   Config_Error : exception;

end Config;
