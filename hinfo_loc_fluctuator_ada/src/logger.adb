--  Logging System Body
--  Thread-safe audit logging implementation

with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Ada.Directories;

package body Logger is

   use Ada.Text_IO;

   --  Global logger state
   Log_File       : File_Type;
   Log_File_Open  : Boolean := False;
   Current_Config : Log_Config;

   --  Protected object for thread-safe logging
   protected Logger_Lock is
      procedure Write_Entry (
         Level   : Log_Level;
         Message : String
      );
   end Logger_Lock;

   protected body Logger_Lock is
      procedure Write_Entry (
         Level   : Log_Level;
         Message : String
      ) is
         Timestamp : constant String :=
            Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
         Level_Str : constant String := Log_Level'Image (Level);
         Full_Message : constant String :=
            "[" & Timestamp & "] [" & Level_Str & "] " & Message;
      begin
         --  Check if we should log this level
         if Log_Level'Pos (Level) < Log_Level'Pos (Current_Config.Level) then
            return;  --  Below configured level
         end if;

         --  Write to console if enabled
         if Current_Config.Console then
            Put_Line (Full_Message);
         end if;

         --  Write to file if open
         if Log_File_Open then
            Put_Line (Log_File, Full_Message);
            Flush (Log_File);  --  Ensure immediate write for security logs

            --  Check file size for rotation
            --  (Simplified - production would be more sophisticated)
            if Natural (Ada.Directories.Size (
               Current_Config.Filename (1 .. Current_Config.Filename_Last)
            )) > Current_Config.Max_Size then
               --  TODO: Implement log rotation
               null;
            end if;
         end if;
      end Write_Entry;
   end Logger_Lock;

   --  Initialize logging system
   procedure Initialize (Config : Log_Config) is
      Filename : constant String := Config.Filename (1 .. Config.Filename_Last);
   begin
      Current_Config := Config;

      if Filename'Length > 0 then
         --  Open or create log file
         begin
            Open (Log_File, Append_File, Filename);
         exception
            when Name_Error =>
               Create (Log_File, Out_File, Filename);
         end;

         Log_File_Open := True;

         --  Log initialization
         Logger_Lock.Write_Entry (Info, "Logging system initialized");
      end if;
   end Initialize;

   --  Log at different levels
   procedure Debug (Message : String) is
   begin
      Logger_Lock.Write_Entry (Debug, Message);
   end Debug;

   procedure Info (Message : String) is
   begin
      Logger_Lock.Write_Entry (Info, Message);
   end Info;

   procedure Warning (Message : String) is
   begin
      Logger_Lock.Write_Entry (Warning, Message);
   end Warning;

   procedure Error (Message : String) is
   begin
      Logger_Lock.Write_Entry (Error, Message);
   end Error;

   procedure Critical (Message : String) is
   begin
      Logger_Lock.Write_Entry (Critical, Message);
   end Critical;

   --  Structured security logging
   procedure Log_Authentication (
      Username : String;
      Success  : Boolean;
      Source   : String := "unknown"
   ) is
      Status : constant String := (if Success then "SUCCESS" else "FAILED");
   begin
      Logger_Lock.Write_Entry (
         (if Success then Info else Warning),
         "AUTH " & Status & " user=" & Username & " source=" & Source
      );
   end Log_Authentication;

   procedure Log_Permission_Check (
      Username   : String;
      Operation  : String;
      Permission : String;
      Granted    : Boolean
   ) is
      Status : constant String := (if Granted then "GRANTED" else "DENIED");
   begin
      Logger_Lock.Write_Entry (
         (if Granted then Debug else Warning),
         "PERMISSION " & Status &
         " user=" & Username &
         " operation=" & Operation &
         " required=" & Permission
      );
   end Log_Permission_Check;

   procedure Log_Record_Generation (
      Domain     : String;
      Record_Type : String;
      Username   : String
   ) is
   begin
      Logger_Lock.Write_Entry (
         Info,
         "GENERATE domain=" & Domain &
         " type=" & Record_Type &
         " user=" & Username
      );
   end Log_Record_Generation;

   procedure Log_Zone_File_Write (
      Filename : String;
      Records  : Natural;
      Success  : Boolean
   ) is
      Status : constant String := (if Success then "SUCCESS" else "FAILED");
   begin
      Logger_Lock.Write_Entry (
         (if Success then Info else Error),
         "ZONEFILE " & Status &
         " file=" & Filename &
         " records=" & Natural'Image (Records)
      );
   end Log_Zone_File_Write;

   procedure Log_Session_Event (
      Event    : String;
      Username : String;
      Session_ID : Natural
   ) is
   begin
      Logger_Lock.Write_Entry (
         Info,
         "SESSION event=" & Event &
         " user=" & Username &
         " sid=" & Natural'Image (Session_ID)
      );
   end Log_Session_Event;

   --  Flush logs
   procedure Flush is
   begin
      if Log_File_Open then
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Flush;

   --  Shutdown logging
   procedure Shutdown is
   begin
      if Log_File_Open then
         Logger_Lock.Write_Entry (Info, "Logging system shutdown");
         Close (Log_File);
         Log_File_Open := False;
      end if;
   end Shutdown;

end Logger;
