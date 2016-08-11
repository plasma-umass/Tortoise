/*
 * Linking an already existing directory throws error
 */

file { '/vagrant/manifests/file/dir1':
  ensure => link,
  target => '/tmp/'
}

file { '/vagrant/manifests/file/dir1/file1':
  ensure => present,
#  before => File ['/vagrant/manifests/file/dir1']
}


file { '/vagrant/manifests/file/dir1/file2':
  ensure => present,
#  before => File ['/vagrant/manifests/file/dir1']
}
