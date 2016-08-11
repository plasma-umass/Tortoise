package { 'python-software-properties':
  ensure => installed
}


exec { 'ppa:webupd8team/java':
  command => '/usr/bin/add-apt-repository ppa:webupd8team/java',
  notify  => Exec ['apt-get update'],
  require => Package ['python-software-properties']
}


exec { 'apt-get update':
  command => '/usr/bin/apt-get update'
}


package { 'oracle-java7-installer':
  require => Exec ['ppa:webupd8team/java'],
  ensure  => installed
}
