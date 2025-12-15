;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — resource-record-fluctuator

(ecosystem
  (version "1.0.0")
  (name "resource-record-fluctuator")
  (type "project")
  (purpose "DNS record randomization tool for deprecated HINFO and LOC records. Built with Ada for maximum security and type safety.")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "DNS record randomization tool for deprecated HINFO and LOC records. Built with Ada for maximum security and type safety.")
  (what-this-is-not "- NOT exempt from RSR compliance"))
