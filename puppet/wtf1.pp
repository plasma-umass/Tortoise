file{'/arjun':
  ensure => file,
  content => "chipmunk"
}

file{'/foo':
  ensure => file,
  content => "baz"
}
