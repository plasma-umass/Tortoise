package { 
  'vim':
    ensure => present
}

class vim($user = $title) {

  user {
    $user:
      ensure => present,
      managehome => true
  }

  file {
    '/home/${user}/.vimrc':
      ensure => present,
      content => "set syntax=on",
      require => Package['vim']
  }
}

vim {
  'awe':
}

vim {
  'arjun':
}
