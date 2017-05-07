$::lsbdistcodename = 'squeeze'
define monit::service($name, $mode) {
  file { "/etc/monit/conf.d/${name}":
    ensure => present,
    owner   => 'root',
    group   => 'root',
    mode    => $mode,
    source  => "puppet:///modules/monit/common/etc/monit/conf.d/${name}",
  }
}
define monit() {
  file { '/etc/default/monit':
    owner   => 'root',
    group   => 'root',
    mode    => '0644',
    alias   => 'monit',
    source  => "puppet:///modules/monit/${::lsbdistcodename}/etc/default/monit",
  }

  file { '/etc/monit/conf.d':
    ensure  => directory,
    force   => true,
    purge   => true,
    recurse => true,
    owner   => 'root',
    group   => 'root',
    mode    => '0644',
    alias   => 'conf.d',
  }

  file { '/etc/monit/monitrc':
    owner   => 'root',
    group   => 'root',
    mode    => '0600',
    alias   => 'monitrc',
    content => "content omitted"
  }

  package { 'monit':
    ensure => present,
  }
}
$service = 'monit'
monit::service {
  name => $service,
  mode => '0644'
}
monit {}