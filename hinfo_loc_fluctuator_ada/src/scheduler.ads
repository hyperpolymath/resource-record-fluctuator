--  Scheduled Fluctuation Module Specification
--  Ada tasking-based periodic DNS record randomization
--
--  Features:
--  - Periodic automatic fluctuation
--  - Configurable intervals
--  - Start/Stop control
--  - Zone file output
--  - Logging integration

with DNS_Records;
with Ada.Real_Time;

package Scheduler is

   --  Fluctuation task control
   type Fluctuation_Mode is (Local_Zone, Remote_Update);

   --  Scheduler configuration
   type Schedule_Config is record
      Domain    : DNS_Records.Domain_Name;
      Interval  : Duration := 3600.0;  --  1 hour default
      Mode      : Fluctuation_Mode := Local_Zone;
      Zone_File : String (1 .. 256) := (others => ' ');
      Zone_File_Last : Natural := 0;
   end record;

   --  Task control
   task type Fluctuation_Task is
      entry Start (Config : Schedule_Config);
      entry Stop;
      entry Pause;
      entry Resume;
      entry Get_Status (
         Running    : out Boolean;
         Iterations : out Natural;
         Last_Run   : out Ada.Real_Time.Time
      );
   end Fluctuation_Task;

   --  Global scheduler instance
   Scheduler_Task : Fluctuation_Task;

   --  Convenience procedures
   procedure Start_Scheduler (Config : Schedule_Config);
   procedure Stop_Scheduler;
   procedure Pause_Scheduler;
   procedure Resume_Scheduler;

   --  Get scheduler statistics
   type Scheduler_Stats is record
      Running        : Boolean;
      Iterations     : Natural;
      Last_Run_Time  : Ada.Real_Time.Time;
   end record;

   function Get_Scheduler_Stats return Scheduler_Stats;

end Scheduler;
