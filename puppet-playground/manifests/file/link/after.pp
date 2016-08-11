file { '/vagrant/manifests/file/link/file1':
  ensure => link,
  target => '/tmp/file1',
/* Cannot supply 'target' and 'content' attributes at the same time */
#  content => 'Some content'
}

file { '/tmp/file1':
  ensure => file,
  content => 'Some content'
}
