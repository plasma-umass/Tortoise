file { '/common':
    ensure => file,
    content => "things",
}

file { '/not':
    ensure => file,
    content => "a",
}
