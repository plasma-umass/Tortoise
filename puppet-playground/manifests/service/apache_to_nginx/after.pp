package { 'apache2':
  ensure => absent
}

file { 'dummy_config':
  path    => '/vagrant/manifests/apache_to_nginx/file1.txt',
  ensure  => absent,
  content => 'Some new content'
}

service { 'apache2':
  ensure => stopped,
  require => Package ['apache2']
}


package { 'nginx':
  ensure => installed
}


service { 'nginx':
  ensure => running,
  require => [
              Package ['nginx'],
              Service ['apache2']
             ]
}
