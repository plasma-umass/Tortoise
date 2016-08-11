package { 'apache2':
  ensure => installed
}

package { 'mysql_auth_module':
  name => 'libapache2-mod-auth-mysql',
  ensure => installed
}


exec { 'enable_mysql_auth_module':
  command => '/usr/sbin/a2enmod auth_mysql',
  notify  => Service ['apache2'],
  require => Package ['mysql_auth_module']
}


service { 'apache2':
  ensure => running,
  require => Package ['apache2']
}
