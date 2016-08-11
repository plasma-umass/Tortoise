user { 'user1':
  ensure => present,
  managehome => true,
}

file { '/home/user1':
  ensure => absent,
  force => true
}
