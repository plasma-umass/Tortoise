file { '/vagrant/manifests/file/dir1':
  ensure => absent,
  force  => true
}

file { '/vagrant/manifests/file/dir1/file1':
  ensure => absent,
}


file { '/vagrant/manifests/file/dir1/file2':
  ensure => absent
}
