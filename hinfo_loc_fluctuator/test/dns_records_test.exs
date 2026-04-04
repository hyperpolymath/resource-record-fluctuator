# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# dns_records_test.exs — Unit, property, and E2E tests for DNS Records module.

defmodule HINFOLOCFluctuator.DNSRecordsTest do
  use ExUnit.Case, async: true

  alias HINFOLOCFluctuator.DNSRecords.HINFO
  alias HINFOLOCFluctuator.DNSRecords.LOC

  # ================================================================
  # Unit tests: HINFO record creation
  # ================================================================

  describe "HINFO.new/3" do
    test "creates HINFO record with required fields" do
      record = HINFO.new("example.com", "x86_64", "Linux")
      assert record.domain == "example.com"
      assert record.cpu == "x86_64"
      assert record.os == "Linux"
    end

    test "HINFO default TTL is 300" do
      record = HINFO.new("test.com", "arm64", "FreeBSD")
      assert record.ttl == 300
    end

    test "HINFO default class is :IN" do
      record = HINFO.new("test.com", "riscv64", "OpenBSD")
      assert record.class == :IN
    end

    test "HINFO custom TTL is respected" do
      record = HINFO.new("test.com", "x86_64", "Linux", ttl: 3600)
      assert record.ttl == 3600
    end

    test "HINFO struct has correct keys" do
      record = HINFO.new("example.com", "x86_64", "Linux")
      assert Map.has_key?(record, :domain)
      assert Map.has_key?(record, :cpu)
      assert Map.has_key?(record, :os)
      assert Map.has_key?(record, :ttl)
      assert Map.has_key?(record, :class)
    end
  end

  # ================================================================
  # Unit tests: LOC record creation
  # ================================================================

  describe "LOC.new/4" do
    test "creates LOC record with coordinates" do
      record = LOC.new("example.com", 51.5, -0.1, 10.0)
      assert record.domain == "example.com"
      assert record.latitude == 51.5
      assert record.longitude == -0.1
      assert record.altitude == 10.0
    end

    test "LOC default TTL is 300" do
      record = LOC.new("test.com", 0.0, 0.0, 0.0)
      assert record.ttl == 300
    end

    test "LOC default class is :IN" do
      record = LOC.new("test.com", 0.0, 0.0, 0.0)
      assert record.class == :IN
    end

    test "LOC struct has required keys" do
      record = LOC.new("test.com", 1.0, 2.0, 3.0)
      assert Map.has_key?(record, :latitude)
      assert Map.has_key?(record, :longitude)
      assert Map.has_key?(record, :altitude)
    end
  end

  # ================================================================
  # Property tests: Invariants over varied inputs
  # ================================================================

  describe "HINFO property invariants" do
    test "domain is always preserved exactly" do
      domains = ["a.com", "test.example.org", "x.y.z.co.uk", "single", "with-hyphen.net"]
      for domain <- domains do
        record = HINFO.new(domain, "cpu", "os")
        assert record.domain == domain
      end
    end

    test "CPU string is always preserved exactly" do
      cpus = ["x86_64", "arm64", "riscv64", "ppc64", "mips", "s390x"]
      for cpu <- cpus do
        record = HINFO.new("test.com", cpu, "Linux")
        assert record.cpu == cpu
      end
    end

    test "TTL is always the value provided" do
      ttls = [60, 300, 900, 3600, 86400]
      for ttl <- ttls do
        record = HINFO.new("test.com", "x86", "Linux", ttl: ttl)
        assert record.ttl == ttl
      end
    end
  end

  describe "LOC property invariants" do
    test "coordinates are preserved exactly" do
      coords = [{0.0, 0.0, 0.0}, {90.0, 180.0, 100.0}, {-90.0, -180.0, -100.0}]
      for {lat, lon, alt} <- coords do
        record = LOC.new("test.com", lat, lon, alt)
        assert record.latitude == lat
        assert record.longitude == lon
        assert record.altitude == alt
      end
    end
  end

  # ================================================================
  # E2E tests: Full record lifecycle
  # ================================================================

  describe "E2E: HINFO zone file format" do
    test "HINFO serializes to zone file format" do
      record = HINFO.new("example.com", "x86_64", "Linux")
      zone = HINFO.to_zone_format(record)
      assert is_binary(zone)
      assert String.contains?(zone, "HINFO")
    end

    test "HINFO zone file contains domain" do
      record = HINFO.new("my.domain.com", "arm64", "FreeBSD")
      zone = HINFO.to_zone_format(record)
      assert String.contains?(zone, "my.domain.com")
    end

    test "HINFO zone file contains CPU and OS" do
      record = HINFO.new("test.com", "riscv64", "NixOS")
      zone = HINFO.to_zone_format(record)
      assert String.contains?(zone, "riscv64")
      assert String.contains?(zone, "NixOS")
    end
  end
end
