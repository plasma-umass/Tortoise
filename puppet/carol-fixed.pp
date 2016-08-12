package{'vim':
  ensure => present
}

file{'/home/carol/.vimrc':
  content => "syntax on"
}

user{'carol':
  ensure => present,
  managehome => true
}

User['carol'] -> File['/home/carol/.vimrc']
