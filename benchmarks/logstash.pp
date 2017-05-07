define logstash::install() {
  
  # contain logstash binary name
  # Change value by your logstash version
  $logstash_binary = 'logstash-1.1.1-monolithic.jar'

  $ensure = 'directory'
  $owner = 'root'
  $group = 'root'
  $mode = '0644'

  file { '/etc/logstash':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }
  file { '/etc/logstash/conf.d':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }
  file { '/usr/local/logstash':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }
  file { '/var/log/logstash':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }
  file { '/etc/init.d':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }
  file { '/etc/logrotate.d':
    ensure => $ensure,
    owner => $owner,
    group => $group,
    mode => $mode,
  }

  # If not exist, deploy run script
  file { '/usr/bin/logstashd':
    ensure => present,
    source => 'puppet:///modules/logstash/logstashd',
    owner => root,
    group => root,
    mode => '0755',
  }

  # If not exist, deploy init script corresponding has $osfamily return
  file { '/etc/init.d/logstash':
    ensure => present,
    source => "puppet:///modules/logstash/logstash_init_${osfamily}",
    owner => root,
    group => root,
    mode => '0755',
  }

  # If not exist, deploy logstash binary
  file { '/usr/local/logstash/logstash.jar':
    ensure => present,
    source => "puppet:///modules/logstash/${logstash_binary}",
    owner => root,
    group => root,
    mode => '0755',
  }

  # If not exist, deploy logrotate configuration file
  file { '/etc/logrotate.d/logstash_rotate':
    ensure => present,
    source => 'puppet:///modules/logstash/logstash_rotate',
    owner => root,
    group => root,
    mode => '0644',
  }

  # If not exist, deploy logstash.log file
  file { '/var/log/logstash/logstash.log':
    ensure => present,
    owner => root,
    group => root,
    mode => '0664',
  }
}

define logstash::conf() {

  # Search base & individual configuration files
  # Restart logstash if necessary

  file {'/etc/logstash/conf.d/logstash_base.conf':
    ensure => present,
    source =>'puppet:///logstash/conf.d/logstash_base.conf',
    owner => root,
    group => root,
    mode => '0644',
  }
}

logstash::install {}
logstash::conf {}