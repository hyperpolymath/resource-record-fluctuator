--  Extended DNS Records Package Body
--  Implementation of DNS record conversions and utilities

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

package body DNS_Records_Extended is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use String_255;
   use String_512;
   use String_1024;
   use String_4096;

   --  ========================================================================
   --  IPv4/IPv6 ADDRESS CONVERSION
   --  ========================================================================

   function To_String (Addr : IPv4_Address) return String is
   begin
      return Trim (Addr (1)'Img, Left) & "." &
             Trim (Addr (2)'Img, Left) & "." &
             Trim (Addr (3)'Img, Left) & "." &
             Trim (Addr (4)'Img, Left);
   end To_String;

   function To_String (Addr : IPv6_Address) return String is
      Result : Unbounded.Unbounded_String;
      Hex_Digits : constant String := "0123456789abcdef";
   begin
      --  Format as standard IPv6 colon-separated hex groups
      for I in 1 .. 8 loop
         declare
            Idx : constant Positive := (I - 1) * 2 + 1;
            High : constant Natural := Natural (Addr (Idx));
            Low  : constant Natural := Natural (Addr (Idx + 1));
            Group : constant Natural := High * 256 + Low;
         begin
            Unbounded.Append (Result, Hex_Digits (Group / 4096 + 1));
            Unbounded.Append (Result, Hex_Digits ((Group / 256) mod 16 + 1));
            Unbounded.Append (Result, Hex_Digits ((Group / 16) mod 16 + 1));
            Unbounded.Append (Result, Hex_Digits (Group mod 16 + 1));
            if I < 8 then
               Unbounded.Append (Result, ':');
            end if;
         end;
      end loop;
      return Unbounded.To_String (Result);
   end To_String;

   function Parse_IPv4 (S : String) return IPv4_Address is
      Result : IPv4_Address := (0, 0, 0, 0);
      Idx : Positive := S'First;
      Octet_Idx : Positive := 1;
      Octet : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) = '.' or I = S'Last then
            if I = S'Last and then S (I) /= '.' then
               Octet := Octet * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
            end if;

            if Octet > 255 then
               raise Constraint_Error with "Invalid IPv4 octet value";
            end if;

            Result (Octet_Idx) := Interfaces.Unsigned_8 (Octet);
            Octet_Idx := Octet_Idx + 1;
            Octet := 0;

            if Octet_Idx > 4 and then I /= S'Last then
               raise Constraint_Error with "Too many octets in IPv4 address";
            end if;
         else
            if S (I) not in '0' .. '9' then
               raise Constraint_Error with "Invalid character in IPv4 address";
            end if;
            Octet := Octet * 10 + (Character'Pos (S (I)) - Character'Pos ('0'));
         end if;
      end loop;

      if Octet_Idx /= 5 then
         raise Constraint_Error with "Incomplete IPv4 address";
      end if;

      return Result;
   end Parse_IPv4;

   function Parse_IPv6 (S : String) return IPv6_Address is
      Result : IPv6_Address := (others => 0);
   begin
      --  Simplified IPv6 parser (full implementation would handle :: compression)
      --  For now, accept full format only
      declare
         Idx : Positive := Result'First;
         Group_Start : Positive := S'First;
         Group_Num : Natural := 0;
      begin
         for I in S'Range loop
            if S (I) = ':' or I = S'Last then
               declare
                  Group_End : constant Positive := (if I = S'Last and S (I) /= ':' then I else I - 1);
                  Group_Str : constant String := S (Group_Start .. Group_End);
                  Value : Natural := 0;
               begin
                  --  Parse hex group
                  for C of Group_Str loop
                     Value := Value * 16;
                     if C in '0' .. '9' then
                        Value := Value + (Character'Pos (C) - Character'Pos ('0'));
                     elsif C in 'a' .. 'f' then
                        Value := Value + (Character'Pos (C) - Character'Pos ('a') + 10);
                     elsif C in 'A' .. 'F' then
                        Value := Value + (Character'Pos (C) - Character'Pos ('A') + 10);
                     else
                        raise Constraint_Error with "Invalid hex character in IPv6";
                     end if;
                  end loop;

                  --  Store as two bytes
                  if Idx + 1 <= Result'Last then
                     Result (Idx) := Interfaces.Unsigned_8 (Value / 256);
                     Result (Idx + 1) := Interfaces.Unsigned_8 (Value mod 256);
                     Idx := Idx + 2;
                  end if;
               end;
               Group_Start := I + 1;
               Group_Num := Group_Num + 1;
            end if;
         end loop;
      end;

      return Result;
   end Parse_IPv6;

   --  ========================================================================
   --  ZONE FILE FORMAT CONVERSION
   --  ========================================================================

   function To_Zone_Format (RR : A_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "A" & ASCII.HT &
             To_String (RR.Address);
   end To_Zone_Format;

   function To_Zone_Format (RR : AAAA_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "AAAA" & ASCII.HT &
             To_String (RR.Address);
   end To_Zone_Format;

   function To_Zone_Format (RR : MX_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "MX" & ASCII.HT &
             Trim (RR.Priority'Img, Left) & " " &
             To_String (RR.Mailserver);
   end To_Zone_Format;

   function To_Zone_Format (RR : NS_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "NS" & ASCII.HT &
             To_String (RR.Nameserver);
   end To_Zone_Format;

   function To_Zone_Format (RR : CNAME_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "CNAME" & ASCII.HT &
             To_String (RR.Target);
   end To_Zone_Format;

   function To_Zone_Format (RR : TXT_Record) return String is
      Text_Str : constant String := To_String (RR.Text);
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "TXT" & ASCII.HT &
             """" & Text_Str & """";
   end To_Zone_Format;

   function To_Zone_Format (RR : SPF_Record) return String is
      Policy_Str : constant String := To_String (RR.Policy);
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "TXT" & ASCII.HT &
             """" & Policy_Str & """";
   end To_Zone_Format;

   function To_Zone_Format (RR : SRV_Record) return String is
   begin
      return To_String (RR.Service) & "." &
             To_String (RR.Protocol) & "." &
             To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "SRV" & ASCII.HT &
             Trim (RR.Priority'Img, Left) & " " &
             Trim (RR.Weight'Img, Left) & " " &
             Trim (RR.Port'Img, Left) & " " &
             To_String (RR.Target);
   end To_Zone_Format;

   function To_Zone_Format (RR : CAA_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "CAA" & ASCII.HT &
             Trim (RR.Flags'Img, Left) & " " &
             To_String (RR.Tag) & " """ &
             To_String (RR.Value) & """";
   end To_Zone_Format;

   function To_Zone_Format (RR : TLSA_Record) return String is
   begin
      return "_" & Trim (RR.Port'Img, Left) & "." &
             To_String (RR.Protocol) & "." &
             To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "TLSA" & ASCII.HT &
             Trim (RR.Usage'Img, Left) & " " &
             Trim (RR.Selector'Img, Left) & " " &
             Trim (RR.Matching_Type'Img, Left) & " " &
             To_String (RR.Certificate_Data);
   end To_Zone_Format;

   function To_Zone_Format (RR : SSHFP_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "SSHFP" & ASCII.HT &
             Trim (RR.Algorithm'Img, Left) & " " &
             Trim (RR.Fingerprint_Type'Img, Left) & " " &
             To_String (RR.Fingerprint);
   end To_Zone_Format;

   function To_Zone_Format (RR : APL_Record) return String is
      Result : Unbounded.Unbounded_String;
   begin
      Unbounded.Append (Result, To_String (RR.Domain));
      Unbounded.Append (Result, ASCII.HT);
      Unbounded.Append (Result, Trim (RR.TTL'Img, Left));
      Unbounded.Append (Result, ASCII.HT & "IN" & ASCII.HT & "APL" & ASCII.HT);

      if RR.Entries /= null then
         for I in RR.Entries'Range loop
            if I > RR.Entries'First then
               Unbounded.Append (Result, " ");
            end if;

            --  Negation prefix
            if RR.Entries (I).Negation then
               Unbounded.Append (Result, "!");
            end if;

            --  Address family: 1=IPv4, 2=IPv6
            if RR.Entries (I).Family = IPv4 then
               Unbounded.Append (Result, "1:");
               --  Format IPv4 from first 4 bytes
               declare
                  Addr : constant IPv4_Address := (
                     RR.Entries (I).Address (1),
                     RR.Entries (I).Address (2),
                     RR.Entries (I).Address (3),
                     RR.Entries (I).Address (4)
                  );
               begin
                  Unbounded.Append (Result, To_String (Addr));
               end;
            else
               Unbounded.Append (Result, "2:");
               Unbounded.Append (Result, To_String (RR.Entries (I).Address));
            end if;

            --  Prefix length
            Unbounded.Append (Result, "/" & Trim (RR.Entries (I).Prefix_Len'Img, Left));
         end loop;
      end if;

      return Unbounded.To_String (Result);
   end To_Zone_Format;

   function To_Zone_Format (RR : DS_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "DS" & ASCII.HT &
             Trim (RR.Key_Tag'Img, Left) & " " &
             Trim (RR.Algorithm'Img, Left) & " " &
             Trim (RR.Digest_Type'Img, Left) & " " &
             To_String (RR.Digest);
   end To_Zone_Format;

   function To_Zone_Format (RR : DNSKEY_Record) return String is
   begin
      return To_String (RR.Domain) & ASCII.HT &
             Trim (RR.TTL'Img, Left) & ASCII.HT &
             "IN" & ASCII.HT &
             "DNSKEY" & ASCII.HT &
             Trim (RR.Flags'Img, Left) & " " &
             Trim (RR.Protocol'Img, Left) & " " &
             Trim (RR.Algorithm'Img, Left) & " " &
             To_String (RR.Public_Key);
   end To_Zone_Format;

   --  ========================================================================
   --  APL RECORD HELPERS
   --  ========================================================================

   function Create_APL_Entry (
      CIDR    : String;
      Negated : Boolean := False
   ) return APL_Entry is
      Slash_Pos : Natural := 0;
      Result : APL_Entry;
   begin
      --  Find the slash separator
      for I in CIDR'Range loop
         if CIDR (I) = '/' then
            Slash_Pos := I;
            exit;
         end if;
      end loop;

      if Slash_Pos = 0 then
         raise Constraint_Error with "Invalid CIDR format (missing /)";
      end if;

      declare
         Addr_Part : constant String := CIDR (CIDR'First .. Slash_Pos - 1);
         Prefix_Part : constant String := CIDR (Slash_Pos + 1 .. CIDR'Last);
         Prefix_Len : Natural;
      begin
         --  Parse prefix length
         Prefix_Len := Natural'Value (Prefix_Part);

         --  Determine if IPv4 or IPv6
         if Index (Addr_Part, ":") > 0 then
            --  IPv6
            Result.Family := IPv6;
            Result.Address := Parse_IPv6 (Addr_Part);
            if Prefix_Len > 128 then
               raise Constraint_Error with "IPv6 prefix length too large";
            end if;
         else
            --  IPv4
            Result.Family := IPv4;
            declare
               IPv4_Addr : constant IPv4_Address := Parse_IPv4 (Addr_Part);
            begin
               Result.Address := (others => 0);
               Result.Address (1 .. 4) := (IPv4_Addr (1), IPv4_Addr (2),
                                           IPv4_Addr (3), IPv4_Addr (4));
            end;
            if Prefix_Len > 32 then
               raise Constraint_Error with "IPv4 prefix length too large";
            end if;
         end if;

         Result.Prefix_Len := Prefix_Len;
         Result.Negation := Negated;
      end;

      return Result;
   end Create_APL_Entry;

   function APL_Matches (
      RR      : APL_Record;
      Address : String
   ) return Boolean is
      Test_Addr : IPv6_Address := (others => 0);
      Is_IPv6 : constant Boolean := Index (Address, ":") > 0;
   begin
      --  Parse the test address
      if Is_IPv6 then
         Test_Addr := Parse_IPv6 (Address);
      else
         declare
            IPv4_Addr : constant IPv4_Address := Parse_IPv4 (Address);
         begin
            Test_Addr (1 .. 4) := (IPv4_Addr (1), IPv4_Addr (2),
                                   IPv4_Addr (3), IPv4_Addr (4));
         end;
      end if;

      --  Check each APL entry
      if RR.Entries /= null then
         for Entry of RR.Entries.all loop
            --  Skip if address family doesn't match
            if (Is_IPv6 and Entry.Family /= IPv6) or
               (not Is_IPv6 and Entry.Family /= IPv4)
            then
               goto Next_Entry;
            end if;

            --  Check if address matches this entry's prefix
            declare
               Matches : Boolean := True;
               Bits_To_Check : constant Natural := Entry.Prefix_Len;
               Bytes_To_Check : constant Natural := Bits_To_Check / 8;
               Remaining_Bits : constant Natural := Bits_To_Check mod 8;
            begin
               --  Check full bytes
               for I in 1 .. Bytes_To_Check loop
                  if Test_Addr (I) /= Entry.Address (I) then
                     Matches := False;
                     exit;
                  end if;
               end loop;

               --  Check remaining bits
               if Matches and Remaining_Bits > 0 then
                  declare
                     Mask : constant Interfaces.Unsigned_8 :=
                        Interfaces.Unsigned_8 (16#FF# - (2 ** (8 - Remaining_Bits) - 1));
                     Byte_Idx : constant Positive := Bytes_To_Check + 1;
                  begin
                     if (Test_Addr (Byte_Idx) and Mask) /=
                        (Entry.Address (Byte_Idx) and Mask)
                     then
                        Matches := False;
                     end if;
                  end;
               end if;

               --  If we have a match, return based on negation
               if Matches then
                  return not Entry.Negation;
               end if;
            end;

            <<Next_Entry>>
         end loop;
      end if;

      --  No match found - default deny
      return False;
   end APL_Matches;

end DNS_Records_Extended;
