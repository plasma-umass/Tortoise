$content = 'Some content'

file { '/vagrant/manifests/file/file2':
  ensure => file,
  content => $content
}

file { '/vagrant/manifests/file/file1':
  ensure => absent,
  content => $content
}
