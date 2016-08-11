package { 'python-oops':
  ensure => absent
}

package { 'python-oops-wsgi':
  ensure  => installed,
  require => Package['python-oops']
}
