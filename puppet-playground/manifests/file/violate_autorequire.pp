file { '/vagrant/manifests/file/dir1':
  ensure => directory
}


file { '/vagrant/manifests/file/dir1/file1':
  ensure => absent,
  before => File ['/vagrant/manifests/file/dir1']
}
