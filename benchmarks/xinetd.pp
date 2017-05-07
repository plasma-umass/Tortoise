$name = 'xinetd'

define xinetd() {
  package { 'xinetd':
    ensure => installed,
  }

  file { '/etc/xinetd.conf':
    ensure => present,
    source => 'puppet:///modules/xinetd/xinetd.conf',
  }
}
define xinetd::service (
  $port,
  $server,
  $ensure         = "present",
  $server_args    = undef,
) {
  file { "/etc/xinetd.d/${name}":
    ensure  => $ensure,
    content => "$server $server_args $port", # more content...
  }
}

$bin = '/usr/bin/rsync'
$args = '--daemon --config /etc/rsync.conf'
$port = '873'
xinetd::service {
  server => $bin,
  server_args => $args,
  port => $port,
}