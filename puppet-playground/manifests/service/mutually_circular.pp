package { 'apache2':
  ensure => installed,
  notify => Service ['a1']
}

/* puppet complains that a1 and a2 refer to the same kind of resource */

service { 'a1':
  name   => 'apache2',
  ensure => running,
  notify => Service ['a2']
}

service { 'a2':
  name   => 'apache2',
  ensure => stopped,
  notify => Service ['a1']
}
