package { 'jre6':
  name => 'openjdk-6-jre',
  ensure => purged
}

package { 'jre7':
  name => 'openjdk-7-jre',
  ensure => purged
}

package { 'scala':
  ensure => installed,
  require => Package ['jre6', 'jre7']
}
