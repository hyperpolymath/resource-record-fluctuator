--  Zone File Writer Module Specification
--  Generates BIND-format DNS zone files for local mode
--
--  Supports:
--  - BIND 9 zone file format
--  - RFC 1035 master file format
--  - Atomic file updates (write to temp, then move)
--  - SOA record generation
--  - HINFO and LOC record serialization

with DNS_Records;
with Ada.Containers.Vectors;

package Zone_Writer is

   --  Zone file configuration
   type Zone_Config is record
      Origin          : DNS_Records.Domain_Name;      --  Zone origin (e.g., "example.com")
      Primary_NS      : DNS_Records.Domain_Name;      --  Primary nameserver
      Hostmaster      : DNS_Records.Domain_Name;      --  Email (admin.example.com)
      Serial          : Natural := 1;                 --  Zone serial number
      Refresh         : DNS_Records.TTL_Seconds := 3600;
      Retry           : DNS_Records.TTL_Seconds := 900;
      Expire          : DNS_Records.TTL_Seconds := 604800;
      Minimum_TTL     : DNS_Records.TTL_Seconds := 300;
   end record;

   --  Resource record vector for batch writing
   package RR_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => DNS_Records.Resource_Record
   );

   --  Write single record to zone file
   procedure Write_Zone_File (
      Filename : String;
      Config   : Zone_Config;
      Records  : RR_Vectors.Vector
   );

   --  Append records to existing zone file
   procedure Append_To_Zone_File (
      Filename : String;
      Records  : RR_Vectors.Vector
   );

   --  Generate SOA record string
   function Generate_SOA (Config : Zone_Config) return String;

   --  Generate zone file header
   function Generate_Header (Config : Zone_Config) return String;

   --  Convert resource record to zone file format
   function To_Zone_Format (RR : DNS_Records.Resource_Record) return String;

   --  Atomic file write (write to temp, then rename)
   procedure Atomic_Write_File (
      Filename : String;
      Content  : String
   );

   --  Increment zone serial (RFC 1912 format: YYYYMMDDnn)
   function Increment_Serial (Current : Natural) return Natural;

end Zone_Writer;
