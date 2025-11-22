--  Firewall Manager Body
--  Implementation of firewall integration and port rotation

with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Interfaces.C;

package body Firewall_Manager is

   use Ada.Calendar;
   use Ada.Text_IO;
   use DNS_Records_Extended;

   --  ========================================================================
   --  INTERNAL STATE
   --  ========================================================================

   Current_Backend : Firewall_Backend := None;

   --  Storage for port rotations
   package Rotation_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Port_Rotation_Config
   );
   Active_Rotations : Rotation_Vectors.Vector;

   --  Storage for service schedules
   package Schedule_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Service_Schedule
   );
   Active_Schedules : Schedule_Vectors.Vector;

   --  Storage for maintenance windows
   package Window_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Maintenance_Window
   );
   Maintenance_Windows : Window_Vectors.Vector;

   --  ========================================================================
   --  PORT ROTATION
   --  ========================================================================

   function Calculate_Current_Port (
      Config : Port_Rotation_Config;
      Time   : Ada.Calendar.Time := Ada.Calendar.Clock
   ) return DNS_Records_Extended.Port_Number is
      use Ada.Calendar.Formatting;
      use type Ada.Real_Time.Time_Span;

      Unix_Epoch : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (
         Year    => 1970,
         Month   => 1,
         Day     => 1,
         Seconds => 0.0
      );
      Elapsed : constant Duration := Time - Unix_Epoch;
      Interval_Seconds : constant Duration :=
         Ada.Real_Time.To_Duration (Config.Rotation_Interval);
      Rotation_Count : constant Natural :=
         Natural (Elapsed / Interval_Seconds);
      Offset : constant Natural := Rotation_Count mod Config.Port_Range;
   begin
      case Config.Strategy is
         when Time_Based =>
            --  Algorithm: port = base_port + (unix_timestamp / interval) % range
            --  Maintainers can calculate this offline
            return Config.Base_Port + Port_Number (Offset);

         when Sequential =>
            return Config.Base_Port + Port_Number (Offset);

         when Random =>
            --  Use rotation count as seed for deterministic "random"
            declare
               Seed_Val : constant Natural := (Rotation_Count * 1103515245 + 12345) mod 2147483647;
            begin
               return Config.Base_Port + Port_Number (Seed_Val mod Config.Port_Range);
            end;

         when Pre_Shared_Key =>
            --  PSK-based algorithm (would need actual PSK implementation)
            --  For now, use time-based as fallback
            return Config.Base_Port + Port_Number (Offset);
      end case;
   end Calculate_Current_Port;

   --  ========================================================================
   --  FIREWALL OPERATIONS
   --  ========================================================================

   procedure Initialize (Backend : Firewall_Backend := Firewalld) is
   begin
      Current_Backend := Backend;
      Put_Line ("[Firewall] Initialized with backend: " & Backend'Image);

      case Backend is
         when Firewalld =>
            --  Check if firewalld is running
            Put_Line ("[Firewall] Checking firewalld status...");

         when IPTables =>
            Put_Line ("[Firewall] Using iptables backend");

         when NFTables =>
            Put_Line ("[Firewall] Using nftables backend");

         when PF =>
            Put_Line ("[Firewall] Using PF backend (BSD)");

         when None =>
            Put_Line ("[Firewall] WARNING: Firewall disabled!");
      end case;
   end Initialize;

   procedure Add_Rule (Rule : Firewall_Rule) is
      use Ada.Strings.Unbounded;
      Command : Unbounded_String;
   begin
      if Current_Backend = None then
         Put_Line ("[Firewall] Skipping rule (firewall disabled): " &
                   To_String (Rule.Name));
         return;
      end if;

      Put_Line ("[Firewall] Adding rule: " & To_String (Rule.Name));

      case Current_Backend is
         when Firewalld =>
            --  Build firewall-cmd command
            Append (Command, "firewall-cmd --permanent --add-rich-rule='rule ");
            Append (Command, "family=""ipv4"" ");
            Append (Command, "source address=""" & To_String (Rule.Source_CIDR) & """ ");
            Append (Command, "port protocol=""" & To_String (Rule.Protocol) & """ ");
            Append (Command, "port=""" & Trim (Rule.Dest_Port'Img, Ada.Strings.Left) & """ ");

            case Rule.Action is
               when Accept | Log_And_Accept =>
                  Append (Command, "accept'");
               when Drop | Log_And_Drop =>
                  Append (Command, "drop'");
               when Reject =>
                  Append (Command, "reject'");
            end case;

            Put_Line ("[Firewall] Command: " & To_String (Command));
            --  Would execute: Execute_Firewalld_Command (To_String (Command));

         when IPTables =>
            Put_Line ("[Firewall] IPTables rule would be added here");

         when NFTables =>
            Put_Line ("[Firewall] NFTables rule would be added here");

         when PF =>
            Put_Line ("[Firewall] PF rule would be added here");

         when None =>
            null;
      end case;
   end Add_Rule;

   procedure Remove_Rule (Rule_Name : String) is
   begin
      Put_Line ("[Firewall] Removing rule: " & Rule_Name);
      --  Implementation would search and remove the rule
   end Remove_Rule;

   procedure Setup_Port_Rotation (Config : Port_Rotation_Config) is
      Current : constant Port_Number := Calculate_Current_Port (Config);
   begin
      Put_Line ("[Port Rotation] Setting up rotation for service: " &
                To_String (Config.Service));
      Put_Line ("[Port Rotation] Strategy: " & Config.Strategy'Image);
      Put_Line ("[Port Rotation] Base port: " &
                Trim (Config.Base_Port'Img, Ada.Strings.Left));
      Put_Line ("[Port Rotation] Range: " &
                Trim (Config.Port_Range'Img, Ada.Strings.Left));
      Put_Line ("[Port Rotation] Current port: " &
                Trim (Current'Img, Ada.Strings.Left));

      --  Store the configuration
      Active_Rotations.Append (Config);

      --  Add firewall rule for current port
      declare
         Rule : Firewall_Rule;
      begin
         Rule.Name := To_Bounded_String (
            To_String (Config.Service) & "_rotation"
         );
         Rule.Source_CIDR := To_Bounded_String ("0.0.0.0/0");
         Rule.Dest_Port := Current;
         Rule.Protocol := To_Bounded_String ("tcp");
         Rule.Action := Accept;
         Rule.Rule_Type := Stateful;

         Add_Rule (Rule);
      end;
   end Setup_Port_Rotation;

   procedure Rotate_Port_Now (Service : String) is
   begin
      Put_Line ("[Port Rotation] Manually rotating port for: " & Service);

      --  Find the service configuration
      for I in Active_Rotations.First_Index .. Active_Rotations.Last_Index loop
         if To_String (Active_Rotations (I).Service) = Service then
            declare
               Config : Port_Rotation_Config := Active_Rotations (I);
               New_Port : constant Port_Number := Calculate_Current_Port (Config);
            begin
               Put_Line ("[Port Rotation] New port: " &
                         Trim (New_Port'Img, Ada.Strings.Left));

               --  Update firewall rules
               --  (Remove old, add new)
            end;
            return;
         end if;
      end loop;

      Put_Line ("[Port Rotation] Service not found: " & Service);
   end Rotate_Port_Now;

   function Get_Current_Port (Service : String) return DNS_Records_Extended.Port_Number is
   begin
      for I in Active_Rotations.First_Index .. Active_Rotations.Last_Index loop
         if To_String (Active_Rotations (I).Service) = Service then
            return Calculate_Current_Port (Active_Rotations (I));
         end if;
      end loop;

      raise Port_Rotation_Error with "Service not found: " & Service;
   end Get_Current_Port;

   --  ========================================================================
   --  SERVICE SCHEDULING
   --  ========================================================================

   procedure Setup_Service_Schedule (Schedule : Service_Schedule) is
   begin
      Put_Line ("[Service Schedule] Setting up schedule for: " &
                To_String (Schedule.Service_Name));
      Put_Line ("[Service Schedule] Protocol: " & To_String (Schedule.Protocol));
      Put_Line ("[Service Schedule] Port: " &
                Trim (Schedule.Port'Img, Ada.Strings.Left));

      Active_Schedules.Append (Schedule);
   end Setup_Service_Schedule;

   procedure Enable_Service (Service_Name : String) is
   begin
      Put_Line ("[Service Schedule] Enabling service: " & Service_Name);

      for I in Active_Schedules.First_Index .. Active_Schedules.Last_Index loop
         if To_String (Active_Schedules (I).Service_Name) = Service_Name then
            declare
               Schedule : constant Service_Schedule := Active_Schedules (I);
               Rule : Firewall_Rule;
            begin
               Rule.Name := To_Bounded_String (Service_Name & "_allow");
               Rule.Source_CIDR := To_Bounded_String ("0.0.0.0/0");
               Rule.Dest_Port := Schedule.Port;
               Rule.Protocol := Schedule.Protocol;
               Rule.Action := Accept;
               Rule.Rule_Type := Schedule.Rule_Type;

               Add_Rule (Rule);
            end;
            return;
         end if;
      end loop;
   end Enable_Service;

   procedure Disable_Service (Service_Name : String) is
   begin
      Put_Line ("[Service Schedule] Disabling service: " & Service_Name);
      Remove_Rule (Service_Name & "_allow");
   end Disable_Service;

   function Is_Service_Active (Service_Name : String) return Boolean is
      Now : constant Time := Clock;
   begin
      for I in Active_Schedules.First_Index .. Active_Schedules.Last_Index loop
         if To_String (Active_Schedules (I).Service_Name) = Service_Name then
            declare
               Schedule : constant Service_Schedule := Active_Schedules (I);
            begin
               --  Check if current time is within any active window
               if Schedule.Windows /= null then
                  for Window of Schedule.Windows.all loop
                     if Window.Window.Enabled and then
                        Now >= Window.Window.Start_Time and then
                        Now <= Window.Window.End_Time
                     then
                        return True;
                     end if;
                  end loop;
               end if;
            end;
            return False;
         end if;
      end loop;

      return False;
   end Is_Service_Active;

   --  ========================================================================
   --  MAINTENANCE WINDOWS
   --  ========================================================================

   procedure Add_Maintenance_Window (Window : Maintenance_Window) is
   begin
      Put_Line ("[Maintenance] Adding window: " & To_String (Window.Description));
      Maintenance_Windows.Append (Window);
   end Add_Maintenance_Window;

   procedure Remove_Maintenance_Window (Description : String) is
   begin
      Put_Line ("[Maintenance] Removing window: " & Description);

      for I in Maintenance_Windows.First_Index .. Maintenance_Windows.Last_Index loop
         if To_String (Maintenance_Windows (I).Description) = Description then
            Maintenance_Windows.Delete (I);
            return;
         end if;
      end loop;
   end Remove_Maintenance_Window;

   function In_Maintenance_Window return Boolean is
      Now : constant Time := Clock;
   begin
      for Window of Maintenance_Windows loop
         if Window.Window.Enabled and then
            Now >= Window.Window.Start_Time and then
            Now <= Window.Window.End_Time
         then
            return True;
         end if;
      end loop;

      return False;
   end In_Maintenance_Window;

   --  ========================================================================
   --  PORT KNOCKING
   --  ========================================================================

   procedure Setup_Port_Knock (Config : Port_Knock_Config) is
   begin
      if not Config.Enabled then
         Put_Line ("[Port Knock] Port knocking disabled");
         return;
      end if;

      Put_Line ("[Port Knock] Setting up port knock for port " &
                Trim (Config.Service_Port'Img, Ada.Strings.Left));

      if Config.Sequence /= null then
         Put ("[Port Knock] Sequence: ");
         for Port of Config.Sequence.all loop
            Put (Trim (Port'Img, Ada.Strings.Left) & " ");
         end loop;
         New_Line;
      end if;

      Put_Line ("[Port Knock] Timeout: " & Config.Timeout'Image & " seconds");
      Put_Line ("[Port Knock] Window open: " & Config.Window_Open'Image & " seconds");
   end Setup_Port_Knock;

   --  ========================================================================
   --  FIREWALLD COMMANDS
   --  ========================================================================

   procedure Execute_Firewalld_Command (Command : String) is
      use GNAT.OS_Lib;
      Args : Argument_List_Access;
      Success : Boolean;
   begin
      Put_Line ("[Firewall] Executing: " & Command);

      --  In real implementation, would execute the command
      --  Args := Argument_String_To_List (Command);
      --  Spawn ("/usr/bin/firewall-cmd", Args.all, Success);

      --  For now, just log
      Put_Line ("[Firewall] (Command execution stubbed for safety)");
   end Execute_Firewalld_Command;

   --  ========================================================================
   --  IP PROTOCOL MODE
   --  ========================================================================

   procedure Set_IP_Protocol_Mode (
      Mode : DNS_Records_Extended.IP_Protocol_Mode
   ) is
   begin
      Put_Line ("[Firewall] Setting IP protocol mode: " & Mode'Image);

      case Mode is
         when IPv4_Only =>
            Put_Line ("[Firewall] Disabling IPv6, enabling IPv4");
            --  Execute_Firewalld_Command ("firewall-cmd --set-default-zone=public");

         when IPv6_Only =>
            Put_Line ("[Firewall] Disabling IPv4, enabling IPv6");

         when Dual_Stack =>
            Put_Line ("[Firewall] Enabling both IPv4 and IPv6");

         when Disabled =>
            Put_Line ("[Firewall] WARNING: Disabling all IP protocols!");
      end case;
   end Set_IP_Protocol_Mode;

   --  ========================================================================
   --  EMERGENCY OPERATIONS
   --  ========================================================================

   procedure Emergency_Lockdown is
   begin
      Put_Line ("[EMERGENCY] INITIATING LOCKDOWN - CLOSING ALL PORTS");

      case Current_Backend is
         when Firewalld =>
            Execute_Firewalld_Command ("firewall-cmd --panic-on");

         when IPTables =>
            Put_Line ("[EMERGENCY] IPTables lockdown would execute here");

         when NFTables =>
            Put_Line ("[EMERGENCY] NFTables lockdown would execute here");

         when PF =>
            Put_Line ("[EMERGENCY] PF lockdown would execute here");

         when None =>
            Put_Line ("[EMERGENCY] Cannot lockdown - firewall disabled");
      end case;
   end Emergency_Lockdown;

   procedure Emergency_Open is
   begin
      Put_Line ("[EMERGENCY] OPENING ALL PORTS - RECOVERY MODE");
      Put_Line ("[EMERGENCY] WARNING: This should only be used for recovery!");

      case Current_Backend is
         when Firewalld =>
            Execute_Firewalld_Command ("firewall-cmd --panic-off");
            Execute_Firewalld_Command ("firewall-cmd --set-default-zone=trusted");

         when IPTables =>
            Put_Line ("[EMERGENCY] IPTables open would execute here");

         when NFTables =>
            Put_Line ("[EMERGENCY] NFTables open would execute here");

         when PF =>
            Put_Line ("[EMERGENCY] PF open would execute here");

         when None =>
            Put_Line ("[EMERGENCY] No action needed - firewall already disabled");
      end case;
   end Emergency_Open;

   --  ========================================================================
   --  STATUS
   --  ========================================================================

   function Get_Firewall_Status return String is
      use Ada.Strings.Unbounded;
      Status : Unbounded_String;
   begin
      Append (Status, "Firewall Status:" & ASCII.LF);
      Append (Status, "  Backend: " & Current_Backend'Image & ASCII.LF);
      Append (Status, "  Active rotations: " &
              Trim (Natural (Active_Rotations.Length)'Img, Ada.Strings.Left) & ASCII.LF);
      Append (Status, "  Active schedules: " &
              Trim (Natural (Active_Schedules.Length)'Img, Ada.Strings.Left) & ASCII.LF);
      Append (Status, "  Maintenance windows: " &
              Trim (Natural (Maintenance_Windows.Length)'Img, Ada.Strings.Left) & ASCII.LF);

      if In_Maintenance_Window then
         Append (Status, "  ** IN MAINTENANCE WINDOW **" & ASCII.LF);
      end if;

      return To_String (Status);
   end Get_Firewall_Status;

end Firewall_Manager;
