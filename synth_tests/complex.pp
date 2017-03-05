class vim($user) {
  package {'vim':
      ensure => present
  }

  user {'$user':
      ensure => present,
      managehome => true
  }

  file {'/home/${user}/.vimrc':
      ensure => present,
      content => 'set syntax=on',
      require => Package['vim']
  }
}

class {'vim':
    user => 'awe'
}
