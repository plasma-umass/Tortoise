/* Fails silenty if /vagrant/manifests/file/dir1 is present */

file { '/vagrant/manifests/file/dir1':
  ensure => absent
}


file { '/vagrant/manifests/file/dir1/file1':
  ensure => present
}


file { '/vagrant/manifests/file/dir1/file2':
  ensure => present
}
