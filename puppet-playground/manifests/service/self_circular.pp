package { 'apache2':
  ensure => installed,
  notify => Service ['a1']
}


/* puppet complains that graph engine found dependency cycle */

service { 'a1':
  name   => 'apache2',
  ensure => running,
  notify => Service ['a1']
}
