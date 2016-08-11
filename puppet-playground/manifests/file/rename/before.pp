file { '/vagrant/manifests/file/file1':
  ensure => file,
  content => 'Some content'
}
