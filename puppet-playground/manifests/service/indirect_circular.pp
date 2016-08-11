package { 'mysql-server':
  ensure => installed,
}


package { 'apache2':
  ensure => installed,
}


service { 'apache2':
  ensure => running,
  notify => Service ['mysql'],
  require => Package ['apache2']
}


service { 'mysql':
  ensure => running,
  require => Package ['mysql-server'],
  notify => Service ['bar']
}


service { 'bar':
  ensure => stopped,
  notify => Service ['apache2']
}
