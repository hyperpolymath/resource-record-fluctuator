--  Logging System Specification
--  Security-focused audit logging for all operations
--
--  Features:
--  - Multiple log levels (Debug, Info, Warning, Error, Critical)
--  - Timestamp on every entry
--  - Thread-safe logging (protected object)
--  - Automatic log rotation
--  - Structured log format

with Ada.Calendar;

package Logger is

   --  Log severity levels
   type Log_Level is (
      Debug,      --  Detailed debugging information
      Info,       --  General informational messages
      Warning,    --  Warning conditions
      Error,      --  Error conditions
      Critical    --  Critical security events
   );

   --  Log configuration
   type Log_Config is record
      Filename      : String (1 .. 256) := (others => ' ');
      Filename_Last : Natural := 0;
      Level         : Log_Level := Info;
      Console       : Boolean := True;    --  Also log to console
      Max_Size      : Natural := 10_000_000;  --  10MB default
   end record;

   --  Initialize logging system
   procedure Initialize (Config : Log_Config);

   --  Log messages at different levels
   procedure Debug (Message : String);
   procedure Info (Message : String);
   procedure Warning (Message : String);
   procedure Error (Message : String);
   procedure Critical (Message : String);

   --  Structured logging for security events
   procedure Log_Authentication (
      Username : String;
      Success  : Boolean;
      Source   : String := "unknown"
   );

   procedure Log_Permission_Check (
      Username   : String;
      Operation  : String;
      Permission : String;
      Granted    : Boolean
   );

   procedure Log_Record_Generation (
      Domain     : String;
      Record_Type : String;
      Username   : String
   );

   procedure Log_Zone_File_Write (
      Filename : String;
      Records  : Natural;
      Success  : Boolean
   );

   procedure Log_Session_Event (
      Event    : String;
      Username : String;
      Session_ID : Natural
   );

   --  Flush logs to disk
   procedure Flush;

   --  Close logging system
   procedure Shutdown;

end Logger;
