package { 'apache2':
  ensure => installed
}

file { 'dummy_config':
  path    => '/vagrant/manifests/apache_to_nginx/file1.txt',
  ensure  => present,
  require => Package ['apache2'],
  notify  => Service ['apache2'],
  content => 'Some content'
}

service { 'apache2':
  ensure => running,
  require => Package ['apache2']
}
