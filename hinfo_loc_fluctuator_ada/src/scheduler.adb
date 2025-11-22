--  Scheduled Fluctuation Module Body
--  Ada tasking implementation for periodic updates

with Ada.Real_Time;
with Randomizer;
with Zone_Writer;
with Logger;
with Ada.Exceptions;

package body Scheduler is

   use Ada.Real_Time;

   --  Fluctuation task implementation
   task body Fluctuation_Task is
      Config       : Schedule_Config;
      Active       : Boolean := False;
      Paused       : Boolean := False;
      Iteration    : Natural := 0;
      Last_Run     : Time := Clock;
      Next_Run     : Time;

      procedure Do_Fluctuation is
         HINFO_RR : DNS_Records.Resource_Record (DNS_Records.HINFO_Type);
         LOC_RR   : DNS_Records.Resource_Record (DNS_Records.LOC_Type);
         Domain_Str : constant String := DNS_Records.Domain_Strings.To_String (Config.Domain);
      begin
         --  Generate random records
         Randomizer.Generate_Quantum_Server (Domain_Str, HINFO_RR, LOC_RR);

         --  Log the generation
         Logger.Info ("SCHEDULER: Generated fluctuation #" & Natural'Image (Iteration));
         Logger.Log_Record_Generation (Domain_Str, "HINFO+LOC", "scheduler");

         --  Perform action based on mode
         case Config.Mode is
            when Local_Zone =>
               --  Write to zone file
               declare
                  Records : Zone_Writer.RR_Vectors.Vector;
                  Zone_Cfg : Zone_Writer.Zone_Config;
                  Filename : constant String :=
                     Config.Zone_File (1 .. Config.Zone_File_Last);
               begin
                  Records.Append (HINFO_RR);
                  Records.Append (LOC_RR);

                  --  Build zone config (simplified)
                  Zone_Cfg.Origin := Config.Domain;
                  Zone_Cfg.Primary_NS := DNS_Records.Domain_Strings.To_Bounded_String ("ns1.example.com");
                  Zone_Cfg.Hostmaster := DNS_Records.Domain_Strings.To_Bounded_String ("admin.example.com");
                  Zone_Cfg.Serial := Zone_Writer.Increment_Serial (Iteration);

                  Zone_Writer.Write_Zone_File (Filename, Zone_Cfg, Records);
                  Logger.Log_Zone_File_Write (Filename, 2, True);
               exception
                  when E : others =>
                     Logger.Error ("Zone file write failed: " &
                        Ada.Exceptions.Exception_Message (E));
                     Logger.Log_Zone_File_Write (Filename, 2, False);
               end;

            when Remote_Update =>
               --  TODO: Implement DNS UPDATE
               Logger.Warning ("Remote DNS UPDATE not yet implemented");
         end case;

         Last_Run := Clock;
         Iteration := Iteration + 1;

      exception
         when E : others =>
            Logger.Error ("Fluctuation failed: " &
               Ada.Exceptions.Exception_Message (E));
      end Do_Fluctuation;

   begin
      loop
         select
            accept Start (Config : Schedule_Config) do
               Fluctuation_Task.Config := Config;
               Active := True;
               Paused := False;
               Iteration := 0;
               Next_Run := Clock + To_Time_Span (Config.Interval);
               Logger.Info ("SCHEDULER: Started with interval " &
                  Duration'Image (Config.Interval) & "s");
            end Start;

         or
            accept Stop do
               Active := False;
               Logger.Info ("SCHEDULER: Stopped after " &
                  Natural'Image (Iteration) & " iterations");
            end Stop;

         or
            accept Pause do
               Paused := True;
               Logger.Info ("SCHEDULER: Paused");
            end Pause;

         or
            accept Resume do
               Paused := False;
               Next_Run := Clock + To_Time_Span (Config.Interval);
               Logger.Info ("SCHEDULER: Resumed");
            end Resume;

         or
            accept Get_Status (
               Running    : out Boolean;
               Iterations : out Natural;
               Last_Run   : out Time
            ) do
               Running := Active and not Paused;
               Iterations := Iteration;
               Last_Run := Fluctuation_Task.Last_Run;
            end Get_Status;

         or
            delay until Next_Run;

            --  Perform fluctuation if active and not paused
            if Active and not Paused then
               Do_Fluctuation;
               Next_Run := Clock + To_Time_Span (Config.Interval);
            end if;

         or
            terminate;
         end select;
      end loop;
   end Fluctuation_Task;

   --  Convenience procedures
   procedure Start_Scheduler (Config : Schedule_Config) is
   begin
      Scheduler_Task.Start (Config);
   end Start_Scheduler;

   procedure Stop_Scheduler is
   begin
      Scheduler_Task.Stop;
   end Stop_Scheduler;

   procedure Pause_Scheduler is
   begin
      Scheduler_Task.Pause;
   end Pause_Scheduler;

   procedure Resume_Scheduler is
   begin
      Scheduler_Task.Resume;
   end Resume_Scheduler;

   --  Get statistics
   function Get_Scheduler_Stats return Scheduler_Stats is
      Stats : Scheduler_Stats;
   begin
      Scheduler_Task.Get_Status (
         Stats.Running,
         Stats.Iterations,
         Stats.Last_Run_Time
      );
      return Stats;
   end Get_Scheduler_Stats;

end Scheduler;
