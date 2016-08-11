file { '/vagrant/manifests/file/link_as_parent/link1':
  ensure => link,
  target => '/tmp/'
}

file {'/vagrant/manifests/file/link_as_parent/link1/file1':
  ensure => file,
  content => 'Some content'
}
