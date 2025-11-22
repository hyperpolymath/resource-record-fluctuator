--  Firewall Manager Specification
--  Integration with firewalld for dynamic access control
--
--  Features:
--  - Time-based maintenance windows
--  - Dynamic port rotation (SSH, services)
--  - CIDR-based restrictions
--  - Stateful/stateless firewall rules
--  - Port knocking integration
--  - Service scheduling (MX, RSS, NNTP with time windows)

with Ada.Calendar;
with Ada.Real_Time;
with DNS_Records_Extended;

package Firewall_Manager is

   --  Firewall backend type
   type Firewall_Backend is (
      Firewalld,   --  Linux firewalld (recommended)
      IPTables,    --  Direct iptables
      NFTables,    --  Linux nftables
      PF,          --  BSD pf
      None         --  No firewall (testing only!)
   );

   --  Firewall rule type
   type Rule_Type is (
      Stateful,    --  Connection tracking
      Stateless    --  No connection tracking (faster)
   );

   --  ========================================================================
   --  TIME-BASED ACCESS CONTROL
   --  ========================================================================

   --  Time window for access
   type Time_Window is record
      Start_Time : Ada.Calendar.Time;
      End_Time   : Ada.Calendar.Time;
      Enabled    : Boolean := True;
   end record;

   --  Maintenance window configuration
   type Maintenance_Window is record
      Window      : Time_Window;
      Description : DNS_Records_Extended.Short_Text;
      Allowed_IPs : DNS_Records_Extended.APL_Entry_Array_Ptr;  --  CIDR ranges
      Ports       : Port_Array_Ptr;  --  Ports to open
   end record;

   type Port_Array is array (Positive range <>) of DNS_Records_Extended.Port_Number;
   type Port_Array_Ptr is access Port_Array;

   type Maintenance_Window_Array is array (Positive range <>) of Maintenance_Window;
   type Maintenance_Window_Ptr is access Maintenance_Window_Array;

   --  ========================================================================
   --  PORT ROTATION
   --  ========================================================================

   --  Port rotation strategy
   type Rotation_Strategy is (
      Sequential,        --  Port 1000, 1001, 1002, ...
      Random,           --  Pseudorandom from pool
      Time_Based,       --  Based on datetime algorithm
      Pre_Shared_Key    --  Algorithm using PSK (maintainers know)
   );

   --  Port rotation configuration
   type Port_Rotation_Config is record
      Service          : DNS_Records_Extended.Short_Text;  --  "ssh", "admin", etc.
      Strategy         : Rotation_Strategy;
      Base_Port        : DNS_Records_Extended.Port_Number := 10000;
      Port_Range       : Natural := 1000;  --  e.g., 10000-10999
      Rotation_Interval : Ada.Real_Time.Time_Span;  --  How often to rotate
      Current_Port     : DNS_Records_Extended.Port_Number;
      Next_Rotation    : Ada.Calendar.Time;
      PSK              : DNS_Records_Extended.Medium_Text;  --  For PSK strategy
   end record;

   --  Calculate current port for time-based rotation
   --  Maintainers can independently calculate this knowing the algorithm
   function Calculate_Current_Port (
      Config : Port_Rotation_Config;
      Time   : Ada.Calendar.Time := Ada.Calendar.Clock
   ) return DNS_Records_Extended.Port_Number;

   --  ========================================================================
   --  SERVICE SCHEDULING
   --  ========================================================================

   --  Service availability schedule
   type Service_Schedule is record
      Service_Name : DNS_Records_Extended.Short_Text;  --  "mx", "smtp", "rss", "nntp"
      Protocol     : DNS_Records_Extended.Short_Text;  --  "tcp", "udp"
      Port         : DNS_Records_Extended.Port_Number;
      Windows      : Maintenance_Window_Ptr;  --  Active time windows
      Randomized   : Boolean := False;  --  Random access windows
      Rule_Type    : Rule_Type := Stateful;
   end record;

   type Service_Schedule_Array is array (Positive range <>) of Service_Schedule;
   type Service_Schedule_Ptr is access Service_Schedule_Array;

   --  ========================================================================
   --  FIREWALL RULE MANAGEMENT
   --  ========================================================================

   --  Firewall rule action
   type Rule_Action is (
      Accept,
      Drop,
      Reject,
      Log_And_Accept,
      Log_And_Drop
   );

   --  Firewall rule
   type Firewall_Rule is record
      Name        : DNS_Records_Extended.Short_Text;
      Source_CIDR : DNS_Records_Extended.Short_Text;
      Dest_Port   : DNS_Records_Extended.Port_Number;
      Protocol    : DNS_Records_Extended.Short_Text;  --  "tcp", "udp", "icmp"
      Action      : Rule_Action;
      Rule_Type   : Rule_Type;
      Priority    : Natural := 100;
      Active      : Boolean := True;
      Time_Window : Time_Window;  --  Optional time restriction
   end record;

   type Firewall_Rule_Array is array (Positive range <>) of Firewall_Rule;
   type Firewall_Rule_Ptr is access Firewall_Rule_Array;

   --  ========================================================================
   --  FIREWALLD INTEGRATION
   --  ========================================================================

   --  Firewalld zone types
   type Firewalld_Zone is (
      Drop,        --  Drop all incoming
      Block,       --  Reject all incoming
      Public,      --  Public-facing
      External,    --  External zone with masquerading
      DMZ,         --  DMZ
      Work,        --  Work environment
      Home,        --  Home environment
      Internal,    --  Internal network
      Trusted      --  Trust all
   );

   --  Firewalld configuration
   type Firewalld_Config is record
      Default_Zone      : Firewalld_Zone := Public;
      Backend           : Firewall_Backend := Firewalld;
      Port_Rotations    : Port_Rotation_Config;
      Service_Schedules : Service_Schedule_Ptr;
      Custom_Rules      : Firewall_Rule_Ptr;
   end record;

   --  ========================================================================
   --  PORT KNOCKING
   --  ========================================================================

   --  Port knocking sequence
   type Knock_Sequence is array (Positive range <>) of DNS_Records_Extended.Port_Number;
   type Knock_Sequence_Ptr is access Knock_Sequence;

   type Port_Knock_Config is record
      Enabled       : Boolean := False;
      Sequence      : Knock_Sequence_Ptr;
      Timeout       : Duration := 5.0;  --  Seconds between knocks
      Window_Open   : Duration := 30.0;  --  How long port stays open
      Service_Port  : DNS_Records_Extended.Port_Number;
   end record;

   --  ========================================================================
   --  FIREWALL OPERATIONS
   --  ========================================================================

   --  Initialize firewall manager
   procedure Initialize (
      Backend : Firewall_Backend := Firewalld
   );

   --  Add/remove firewall rules
   procedure Add_Rule (Rule : Firewall_Rule);
   procedure Remove_Rule (Rule_Name : String);

   --  Port rotation operations
   procedure Setup_Port_Rotation (Config : Port_Rotation_Config);
   procedure Rotate_Port_Now (Service : String);
   function Get_Current_Port (Service : String) return DNS_Records_Extended.Port_Number;

   --  Service scheduling
   procedure Setup_Service_Schedule (Schedule : Service_Schedule);
   procedure Enable_Service (Service_Name : String);
   procedure Disable_Service (Service_Name : String);
   function Is_Service_Active (Service_Name : String) return Boolean;

   --  Maintenance windows
   procedure Add_Maintenance_Window (Window : Maintenance_Window);
   procedure Remove_Maintenance_Window (Description : String);
   function In_Maintenance_Window return Boolean;

   --  Port knocking
   procedure Setup_Port_Knock (Config : Port_Knock_Config);

   --  Execute firewalld commands
   procedure Execute_Firewalld_Command (Command : String);

   --  IPv4/IPv6 toggle
   procedure Set_IP_Protocol_Mode (
      Mode : DNS_Records_Extended.IP_Protocol_Mode
   );

   --  Emergency shutdown (close all ports)
   procedure Emergency_Lockdown;

   --  Emergency open (allow all - for recovery)
   procedure Emergency_Open;

   --  Get firewall status
   function Get_Firewall_Status return String;

   --  Exceptions
   Firewall_Error : exception;
   Port_Rotation_Error : exception;

end Firewall_Manager;
