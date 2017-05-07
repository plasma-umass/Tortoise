define repos() {
  file { '/etc/yum.repos.d/epel-apache-maven.repo':
    source => 'puppet:///modules/bucket/etc/yum.repos.d/epel-apache-maven.repo',
  }

  file { '/etc/yum.repos.d/CentOS-Base.repo':
    source => 'puppet:///modules/bucket/etc/yum.repos.d/CentOS-Base.repo',
  }
}
define packages($ensure) {
  package { 'java-1.6.0-openjdk-devel':
    ensure => $ensure,
  }
  package { 'unzip':
    ensure => $ensure,
  }
  package { 'ant':
    ensure => $ensure,
  }
  package { 'vim-enhanced':
    ensure => $ensure,
  }
}
define sysprep() {
  $owner = 'root'
  $group = 'root'
  $mode = '0644'
  file { '/etc/inittab':
    source => 'puppet:///modules/bucket/etc/inittab',
    owner  => $owner,
    group  => $group,
    mode   => $mode,
  }
}
repos {}
$flag = 'present'
packages { ensure => $flag }
sysprep {}