user { 'aaron':
    name => 'aaron',
    ensure => present,
    managehome => true,
}

user { 'web':
    name => 'web',
    ensure => present,
    managehome => false,
}

user { 'irc':
    name => 'irc',
    ensure => present,
    managehome => false,
}

package { 'vim':
    ensure => present,
    before => File['/home/aaron.vimrc'],
}

file { '/home/aaron/.vimrc':
    name => "/home/aaron/.vimrc",
    ensure => present,
    content => "something",
    require => User['aaron'],
}
