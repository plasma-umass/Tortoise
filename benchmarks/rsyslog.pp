$::osfamily = "debian"

define rsyslog (
    $package_ensure = "present",
    $default_config = 'false',
) { 

  $config_file = '/etc/rsyslog.conf'
  $config_dir = '/etc/rsyslog.d'
  $package_name = 'rsyslog'
  $default_owner = "root"
  $default_group = "root"
  $service_name = 'rsyslog'

  package { $package_name:
    ensure => $package_ensure,
  }

  if $::osfamily == 'openbsd' { 
    file { "/etc/rc.d/${service_name}":
      ensure => present,
      owner => $default_owner,
      group => $default_group,
      mode => '0744',
      source => 'puppet:///modules/rsyslog/rsyslogd.rc',
    }
  }

  file { $config_file:
    ensure => present,
    owner => $default_owner,
    group => $default_group,
    mode => '0444',
    content => "content omitted",
  }

  # note: all unmanaged config snippets will be discarded.
  file { $config_dir:
    ensure => 'directory',
    owner => $default_owner,
    group => $default_group,
    mode => '0755',
    recurse => true,
    purge => true,
    force => true,
  }

  if $default_config == 'true' {
    rsyslog::snippet { '50-default':
      name => $package_name,
      lines => "'*.info;mail.none;authpriv.none;cron.none               /var/log/messages',
        'kern.*                      -/var/log/kern.log',
        'auth.*;authpriv.*           /var/log/auth.log',
        'daemon.*                    /var/log/daemon.log',
        'cron.*                      -/var/log/cron.log',
        'mail.*                      -/var/log/mail.log',
        'uucp,news.*                 /var/log/spooler',
        '*.emerg                     *',
        'local7.*                    /var/log/boot.log',
        '*.*                         /var/log/uncategorized.log'"
    }
  }
}

define rsyslog::snippet (
  $name,
  $lines,
  $configdir
) {
  file { "$configdir/${name}.conf":
    ensure => present,
    owner => $default_owner,
    group => $default_group,
    mode => '0444',
    content => $lines,
  }
}

$ensure = 'present'
$flag = 'true'
rsyslog {
  package_ensure => $ensure,
  default_config => $flag,
}