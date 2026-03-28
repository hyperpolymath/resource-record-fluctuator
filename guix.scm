; SPDX-License-Identifier: PMPL-1.0-or-later
;; guix.scm — GNU Guix package definition for resource-record-fluctuator
;; Usage: guix shell -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "resource-record-fluctuator")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (synopsis "resource-record-fluctuator")
  (description "resource-record-fluctuator — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/resource-record-fluctuator")
  (license ((@@ (guix licenses) license) "PMPL-1.0-or-later"
             "https://github.com/hyperpolymath/palimpsest-license")))
