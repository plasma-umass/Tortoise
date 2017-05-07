$module_name = 'ntp'

define ntp (
  $package_name = "ntp",
  $service_name = "ntpd",
  $config_file  = '/etc/ntp.conf',
  $template     = "ntp/ntp.conf-rhel.erb", # NOTE(arjun): removed $module_name
  $logfile      = 'false',
) { 

  # Main package and service it provides
  package { $package_name: ensure => 'installed' }

  # Main configuration file
  file { $config_file:
    ensure  => present,
    owner   => 'root',
    group   => 'root',
    mode    => '0644',
    content => $template,
  }

  if ($logfile != 'false') and ($service_name == 'ntpd') {
    # Logrotate for our custom log file
    file { '/etc/logrotate.d/ntpd':
      ensure  => present,
      owner   => 'root',
      group   => 'root',
      mode    => '0644',
      content => "${module_name}/ntpd-logrotate.erb",
    }
  }
}

$package = 'ntp'
$service = 'ntpd'
$conf = '/etc/ntp.conf'
$flag = 'true'
ntp {
  package_name => $package,
  service_name => $service,
  config_file => $conf,
  logfile => $flag
}