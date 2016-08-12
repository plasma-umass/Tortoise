define cpp() {
  if !defined(Package['m4']) {
    package {'m4': ensure => present }
  }
  if !defined(Package['make']) {
    package {'make': ensure => present }
  }
  package {'gcc': ensure => present }
  Package['m4'] -> Package['make']
  Package['make'] -> Package['gcc']
}

define ocaml() {
  if !defined(Package['m4']) {
    package {'m4': ensure => present }
  }
  if !defined(Package['make']) {
    package {'make': ensure => present }
  }
  package {'ocaml': ensure => present }
  Package['make'] -> Package['m4']
  Package['m4'] -> Package['ocaml']
}

cpp{"mycpp": }
ocaml{"myocaml": }
