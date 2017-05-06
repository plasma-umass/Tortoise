# From https://github.com/thias/puppet-bind

$::bind::params::packagenameprefix = 'bind9'
$::bind::params::binduser          = 'bind'
$::bind::params::bindgroup         = 'bind'

define bind::package (
  $packagenameprefix = $::bind::params::packagenameprefix,
  $packagenamesuffix = '',
) {
  package { "$packagenameprefix$packagenamesuffix":
    ensure => present
  }
}

define bind(
  $chroot = 'false',
) {
  if $chroot == 'true' {
    bind::package {
      packagenamesuffix => "-chroot"
    }

    file {'/var/named/chroot/var/log/named':
      ensure  => directory,
      owner   => $::bind::params::binduser,
      group   => $::bind::params::bindgroup,
      mode    => '0770',
      seltype => 'var_log_t',
    }
  } else { if $chroot == 'false' {
    bind::package {
      packagenamesuffix => ""
    }
    
    file {'/var/log/named':
      ensure  => directory,
      owner   => $::bind::params::binduser,
      group   => $::bind::params::bindgroup,
      mode    => '0770',
      seltype => 'var_log_t',
    }
  } }
}

$shouldchroot = 'false'
bind {
  chroot => $shouldchroot
}