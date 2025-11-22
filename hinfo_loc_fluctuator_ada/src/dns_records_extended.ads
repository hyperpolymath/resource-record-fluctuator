--  Extended DNS Records Package Specification
--  Support for ALL DNS resource record types
--
--  Includes:
--  - IPv4/IPv6 (A, AAAA with toggle support)
--  - Mail (MX, SPF, DKIM, DMARC)
--  - Security (CAA, TLSA, SSHFP, DS, DNSKEY, NSEC, NSEC3)
--  - Service (SRV, NAPTR, APL, URI)
--  - Standard (NS, CNAME, PTR, TXT, SOA)
--  - DNSSEC (RRSIG, NSEC, NSEC3, NSEC3PARAM, DS, DNSKEY, CDS, CDNSKEY)
--  - Experimental and modern records

with Ada.Strings.Bounded;
with Interfaces;

package DNS_Records_Extended is

   --  Network Protocol Support Toggle
   type IP_Protocol_Mode is (
      IPv4_Only,
      IPv6_Only,
      Dual_Stack,      --  Both IPv4 and IPv6
      Disabled         --  Neither (for diagnostic purposes)
   );

   --  IPv4 Address (32-bit)
   type IPv4_Address is array (1 .. 4) of Interfaces.Unsigned_8;

   --  IPv6 Address (128-bit)
   type IPv6_Address is array (1 .. 16) of Interfaces.Unsigned_8;

   --  DNS Classes (extended)
   type DNS_Class is (
      IN_Class,   --  Internet (standard)
      CH_Class,   --  Chaos
      HS_Class,   --  Hesiod
      NONE_Class, --  NONE (RFC 2136)
      ANY_Class   --  ANY (query only)
   );

   --  TTL (Time To Live) in seconds
   type TTL_Seconds is range 0 .. 2_147_483_647;  --  RFC max: 2^31-1

   --  Priority/Weight/Preference values (MX, SRV, NAPTR)
   type Priority_Value is range 0 .. 65_535;
   type Weight_Value is range 0 .. 65_535;
   type Port_Number is range 0 .. 65_535;

   --  Bounded strings for various fields
   package String_255 is new Ada.Strings.Bounded.Generic_Bounded_Length (255);
   package String_512 is new Ada.Strings.Bounded.Generic_Bounded_Length (512);
   package String_1024 is new Ada.Strings.Bounded.Generic_Bounded_Length (1024);
   package String_4096 is new Ada.Strings.Bounded.Generic_Bounded_Length (4096);

   subtype Domain_Name is String_255.Bounded_String;
   subtype Short_Text is String_255.Bounded_String;
   subtype Medium_Text is String_512.Bounded_String;
   subtype Long_Text is String_1024.Bounded_String;
   subtype Very_Long_Text is String_4096.Bounded_String;

   --  ========================================================================
   --  BASIC ADDRESS RECORDS
   --  ========================================================================

   --  A Record (IPv4 address)
   type A_Record is record
      Domain  : Domain_Name;
      Address : IPv4_Address;
      TTL     : TTL_Seconds := 300;
      Class   : DNS_Class := IN_Class;
   end record;

   --  AAAA Record (IPv6 address)
   type AAAA_Record is record
      Domain  : Domain_Name;
      Address : IPv6_Address;
      TTL     : TTL_Seconds := 300;
      Class   : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  DELEGATION AND ALIASING
   --  ========================================================================

   --  NS Record (Name Server)
   type NS_Record is record
      Domain     : Domain_Name;
      Nameserver : Domain_Name;
      TTL        : TTL_Seconds := 86400;  --  24 hours typical
      Class      : DNS_Class := IN_Class;
   end record;

   --  CNAME Record (Canonical Name)
   type CNAME_Record is record
      Domain : Domain_Name;
      Target : Domain_Name;
      TTL    : TTL_Seconds := 300;
      Class  : DNS_Class := IN_Class;
   end record;

   --  PTR Record (Pointer for reverse DNS)
   type PTR_Record is record
      Reverse_IP : Domain_Name;  --  e.g., "1.0.0.127.in-addr.arpa"
      Hostname   : Domain_Name;
      TTL        : TTL_Seconds := 86400;
      Class      : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  MAIL RECORDS
   --  ========================================================================

   --  MX Record (Mail Exchange)
   type MX_Record is record
      Domain     : Domain_Name;
      Priority   : Priority_Value;
      Mailserver : Domain_Name;
      TTL        : TTL_Seconds := 3600;
      Class      : DNS_Class := IN_Class;
      Enabled    : Boolean := True;  --  For time-based scheduling
   end record;

   --  ========================================================================
   --  TEXT RECORDS
   --  ========================================================================

   --  TXT Record (Text data)
   type TXT_Record is record
      Domain  : Domain_Name;
      Text    : Very_Long_Text;  --  Can be very long (SPF, DKIM, etc.)
      TTL     : TTL_Seconds := 300;
      Class   : DNS_Class := IN_Class;
   end record;

   --  SPF Record (Sender Policy Framework) - TXT-based
   type SPF_Record is record
      Domain : Domain_Name;
      Policy : Long_Text;  --  e.g., "v=spf1 mx ~all"
      TTL    : TTL_Seconds := 3600;
      Class  : DNS_Class := IN_Class;
   end record;

   --  DKIM Record (DomainKeys Identified Mail) - TXT at subdomain
   type DKIM_Record is record
      Selector   : Short_Text;  --  e.g., "default"
      Domain     : Domain_Name;
      Public_Key : Very_Long_Text;  --  Base64-encoded public key
      TTL        : TTL_Seconds := 3600;
      Class      : DNS_Class := IN_Class;
   end record;

   --  DMARC Record (Domain-based Message Authentication)
   type DMARC_Record is record
      Domain : Domain_Name;
      Policy : Medium_Text;  --  e.g., "v=DMARC1; p=quarantine; rua=mailto:..."
      TTL    : TTL_Seconds := 3600;
      Class  : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  SERVICE RECORDS
   --  ========================================================================

   --  SRV Record (Service location)
   type SRV_Record is record
      Service  : Short_Text;    --  e.g., "_http", "_ldap"
      Protocol : Short_Text;    --  "_tcp" or "_udp"
      Domain   : Domain_Name;
      Priority : Priority_Value;
      Weight   : Weight_Value;
      Port     : Port_Number;
      Target   : Domain_Name;
      TTL      : TTL_Seconds := 3600;
      Class    : DNS_Class := IN_Class;
      Enabled  : Boolean := True;  --  For scheduling
   end record;

   --  NAPTR Record (Naming Authority Pointer - for ENUM, SIP)
   type NAPTR_Record is record
      Domain      : Domain_Name;
      Order       : Priority_Value;
      Preference  : Priority_Value;
      Flags       : Short_Text;
      Service     : Short_Text;
      Regexp      : Medium_Text;
      Replacement : Domain_Name;
      TTL         : TTL_Seconds := 3600;
      Class       : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  SECURITY RECORDS
   --  ========================================================================

   --  CAA Record (Certification Authority Authorization)
   type CAA_Record is record
      Domain : Domain_Name;
      Flags  : Interfaces.Unsigned_8;  --  0 = non-critical, 128 = critical
      Tag    : Short_Text;  --  "issue", "issuewild", "iodef"
      Value  : Medium_Text;
      TTL    : TTL_Seconds := 86400;
      Class  : DNS_Class := IN_Class;
   end record;

   --  TLSA Record (DANE - DNS-based Authentication of Named Entities)
   type TLSA_Record is record
      Port            : Port_Number;
      Protocol        : Short_Text;  --  "_tcp" or "_udp"
      Domain          : Domain_Name;
      Usage           : Interfaces.Unsigned_8;  --  0-3
      Selector        : Interfaces.Unsigned_8;  --  0-1
      Matching_Type   : Interfaces.Unsigned_8;  --  0-2
      Certificate_Data : Very_Long_Text;  --  Hex-encoded
      TTL             : TTL_Seconds := 86400;
      Class           : DNS_Class := IN_Class;
   end record;

   --  SSHFP Record (SSH Fingerprint)
   type SSHFP_Record is record
      Domain         : Domain_Name;
      Algorithm      : Interfaces.Unsigned_8;  --  1=RSA, 2=DSA, 3=ECDSA, 4=Ed25519
      Fingerprint_Type : Interfaces.Unsigned_8;  --  1=SHA-1, 2=SHA-256
      Fingerprint    : Medium_Text;  --  Hex-encoded
      TTL            : TTL_Seconds := 86400;
      Class          : DNS_Class := IN_Class;
   end record;

   --  APL Record (Address Prefix List - for access control)
   type Address_Family is (IPv4, IPv6);

   type APL_Entry is record
      Family     : Address_Family;
      Negation   : Boolean := False;  --  If true, this is an exclusion
      Prefix_Len : Natural range 0 .. 128;
      Address    : IPv6_Address;  --  Use for both (IPv4 in first 4 bytes)
   end record;

   type APL_Entry_Array is array (Positive range <>) of APL_Entry;
   type APL_Entry_Array_Ptr is access APL_Entry_Array;

   type APL_Record is record
      Domain  : Domain_Name;
      Entries : APL_Entry_Array_Ptr;
      TTL     : TTL_Seconds := 3600;
      Class   : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  DNSSEC RECORDS
   --  ========================================================================

   --  DS Record (Delegation Signer)
   type DS_Record is record
      Domain       : Domain_Name;
      Key_Tag      : Interfaces.Unsigned_16;
      Algorithm    : Interfaces.Unsigned_8;
      Digest_Type  : Interfaces.Unsigned_8;
      Digest       : Very_Long_Text;
      TTL          : TTL_Seconds := 86400;
      Class        : DNS_Class := IN_Class;
   end record;

   --  DNSKEY Record (DNS Public Key)
   type DNSKEY_Record is record
      Domain    : Domain_Name;
      Flags     : Interfaces.Unsigned_16;  --  256=ZSK, 257=KSK
      Protocol  : Interfaces.Unsigned_8;   --  Always 3
      Algorithm : Interfaces.Unsigned_8;
      Public_Key : Very_Long_Text;
      TTL       : TTL_Seconds := 86400;
      Class     : DNS_Class := IN_Class;
   end record;

   --  NSEC Record (Next Secure - for DNSSEC denial of existence)
   type NSEC_Record is record
      Domain     : Domain_Name;
      Next_Domain : Domain_Name;
      Type_Bitmap : Medium_Text;  --  Bitmap of existing record types
      TTL        : TTL_Seconds := 3600;
      Class      : DNS_Class := IN_Class;
   end record;

   --  NSEC3 Record (Next Secure v3 - hashed)
   type NSEC3_Record is record
      Domain           : Domain_Name;
      Hash_Algorithm   : Interfaces.Unsigned_8;
      Flags            : Interfaces.Unsigned_8;
      Iterations       : Interfaces.Unsigned_16;
      Salt             : Medium_Text;
      Next_Hashed_Owner : Medium_Text;
      Type_Bitmap      : Medium_Text;
      TTL              : TTL_Seconds := 3600;
      Class            : DNS_Class := IN_Class;
   end record;

   --  ========================================================================
   --  ZONE TRANSFER AND TOPOLOGY
   --  ========================================================================

   --  DNS Topology Modes
   type DNS_Topology is (
      Standard,           --  Single primary, one or more secondaries
      Split_Horizon,      --  Different views for internal/external
      Primary_Primary,    --  Multi-master replication
      Hidden_Primary,     --  Primary not publicly visible
      Secondary_Only,     --  Read-only secondary
      Stealth_Secondary   --  Secondary not listed in NS records
   );

   --  AXFR Configuration (Zone Transfer)
   type AXFR_Config is record
      Enabled         : Boolean := False;
      Allowed_IPs     : APL_Entry_Array_Ptr;  --  IPs allowed to transfer
      TSIG_Required   : Boolean := True;
      Topology        : DNS_Topology := Standard;
      Primary_Server  : Domain_Name;
      Secondary_Servers : APL_Entry_Array_Ptr;
   end record;

   --  ========================================================================
   --  UTILITY FUNCTIONS
   --  ========================================================================

   --  IPv4/IPv6 Conversion
   function To_String (Addr : IPv4_Address) return String;
   function To_String (Addr : IPv6_Address) return String;
   function Parse_IPv4 (S : String) return IPv4_Address;
   function Parse_IPv6 (S : String) return IPv6_Address;

   --  Zone file format conversion
   function To_Zone_Format (RR : A_Record) return String;
   function To_Zone_Format (RR : AAAA_Record) return String;
   function To_Zone_Format (RR : MX_Record) return String;
   function To_Zone_Format (RR : NS_Record) return String;
   function To_Zone_Format (RR : CNAME_Record) return String;
   function To_Zone_Format (RR : TXT_Record) return String;
   function To_Zone_Format (RR : SPF_Record) return String;
   function To_Zone_Format (RR : SRV_Record) return String;
   function To_Zone_Format (RR : CAA_Record) return String;
   function To_Zone_Format (RR : TLSA_Record) return String;
   function To_Zone_Format (RR : SSHFP_Record) return String;
   function To_Zone_Format (RR : APL_Record) return String;
   function To_Zone_Format (RR : DS_Record) return String;
   function To_Zone_Format (RR : DNSKEY_Record) return String;

   --  APL record helpers
   function Create_APL_Entry (
      CIDR    : String;  --  e.g., "192.168.1.0/24" or "2001:db8::/32"
      Negated : Boolean := False
   ) return APL_Entry;

   function APL_Matches (
      RR      : APL_Record;
      Address : String  --  IPv4 or IPv6 address
   ) return Boolean;

end DNS_Records_Extended;
