package { 'apache2':
  ensure => installed
}

file { 'dummy_conf':
  path    => '/vagrant/manifests/apache_conf/file1.txt',
  ensure  => present,
  content => 'original content'
}

service { 'apache2':
  ensure => running,
  require => Package ['apache2'],
  subscribe => File ['dummy_conf']
}
