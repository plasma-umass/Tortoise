file { '/mydir': 
  ensure => 'directory'
}
file { '/mydir/myfile':
  ensure => 'file'
  require => File['/mydir']
}