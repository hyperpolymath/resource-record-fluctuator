--  DNS UPDATE Module Specification (RFC 2136)
--  Dynamic DNS update protocol implementation
--
--  Status: PARTIAL IMPLEMENTATION
--  This is a framework showing the intended structure.
--  Full DNS wire format encoding and TSIG support needed for production.
--
--  References:
--  - RFC 2136: Dynamic Updates in the Domain Name System
--  - RFC 2845: Secret Key Transaction Authentication for DNS (TSIG)
--  - RFC 1035: Domain Names - Implementation and Specification

with DNS_Records;
with Ada.Streams;

package DNS_Update is

   --  DNS message opcodes
   type Opcode is (
      Query,   --  Standard query (0)
      IQuery,  --  Inverse query (1) - obsolete
      Status,  --  Server status (2)
      Update   --  Dynamic update (5)
   );

   --  DNS response codes
   type Response_Code is (
      NoError,       --  No error (0)
      FormErr,       --  Format error (1)
      ServFail,      --  Server failure (2)
      NXDomain,      --  Non-existent domain (3)
      NotImp,        --  Not implemented (4)
      Refused,       --  Query refused (5)
      YXDomain,      --  Name exists when it should not (6)
      YXRRSet,       --  RR set exists when it should not (7)
      NXRRSet,       --  RR set that should exist does not (8)
      NotAuth,       --  Server not authoritative (9)
      NotZone        --  Name not contained in zone (10)
   );

   --  Update operation types
   type Update_Operation is (
      Add,      --  Add RR to zone
      Delete,   --  Delete RR from zone
      DeleteAll --  Delete all RRs with name
   );

   --  DNS UPDATE message structure (simplified)
   type Update_Message is record
      Transaction_ID : Natural range 0 .. 65535;
      Zone_Name      : DNS_Records.Domain_Name;
      Operation      : Update_Operation;
      Record_Data    : DNS_Records.Resource_Record;
      TSIG_Signed    : Boolean := False;
   end record;

   --  TSIG (Transaction Signature) configuration
   type TSIG_Config is record
      Key_Name    : DNS_Records.Domain_Name;
      Algorithm   : String (1 .. 64) := (others => ' ');
      Alg_Last    : Natural := 0;
      Secret      : String (1 .. 256) := (others => ' ');
      Secret_Last : Natural := 0;
   end record;

   --  DNS UPDATE client configuration
   type Update_Config is record
      Server_Address : String (1 .. 256) := (others => ' ');
      Addr_Last      : Natural := 0;
      Server_Port    : Natural := 53;
      Use_TSIG       : Boolean := False;
      TSIG           : TSIG_Config;
      Timeout        : Duration := 5.0;
   end record;

   --  Send DNS UPDATE message
   procedure Send_Update (
      Config  : Update_Config;
      Message : Update_Message;
      Success : out Boolean;
      Error   : out Response_Code
   );

   --  High-level update operations
   procedure Add_HINFO_Record (
      Config : Update_Config;
      Zone   : String;
      Record : DNS_Records.Resource_Record;
      Success : out Boolean
   );

   procedure Add_LOC_Record (
      Config : Update_Config;
      Zone   : String;
      Record : DNS_Records.Resource_Record;
      Success : out Boolean
   );

   procedure Delete_Records (
      Config : Update_Config;
      Zone   : String;
      Domain : String;
      Success : out Boolean
   );

   --  TSIG operations (placeholder for future implementation)
   procedure Load_TSIG_Key (
      Filename : String;
      Config   : out TSIG_Config
   );

   function Sign_Message (
      Message : Update_Message;
      TSIG    : TSIG_Config
   ) return Ada.Streams.Stream_Element_Array;

   --  DNS UPDATE exceptions
   Update_Error : exception;
   TSIG_Error   : exception;
   Network_Error : exception;

end DNS_Update;
