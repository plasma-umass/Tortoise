define hosting::mysql() {
  package { 'mysql-server':
    ensure => present,
  }
}

define hosting::php() {
  package { 'php5-fpm':
    ensure => present,
  }
  package { 'php5-mysql':
    ensure => present,
  }
  package { 'php-apc':
    ensure => present,
  }
  package { 'php5-gd':
    ensure => present,
  }
  package { 'php5-curl':
    ensure => present,
  }
}

define hosting() {
  package { 'nginx':
    ensure => present
  }
  file { 'www':
    ensure => directory,
    path   => '/www',
  }
  file { 'w3tc.inc':
    ensure => file,
    path   => '/etc/nginx/sites-available/w3tc.inc',
    source => 'puppet:///modules/hosting/w3tc.inc',
  }
  file { 'cloudflare.inc':
    ensure => file,
    path   => '/etc/nginx/sites-available/cloudflare.inc',
    source => 'puppet:///modules/hosting/cloudflare.inc',
  }
}
define site(
  $type, $url, $url_aliases, $contact, $ip='NONE',
  $custom_document_root='false', $database='NONE',
  $cloudflare='false', $upstream='NONE'
) {
  $www_root = "/www/${url}"
  if $custom_document_root == 'false' {
    file { $www_root:
      ensure => directory,
      path   => $www_root,
    }
  }
  if $type == 'custom' {
    file { "vhost_${url}":
      ensure  => file,
      path    => "/etc/nginx/sites-available/${url}.conf",
    }
  } else {
    file { "vhost_${url}":
      ensure  => file,
      path    => "/etc/nginx/sites-available/${url}.conf",
      content => "content omitted",
    }
  }
  file { "activate_${url}":
    ensure  => link,
    path    => "/etc/nginx/sites-enabled/${url}.conf",
    target  => "/etc/nginx/sites-available/${url}.conf",
  }
  file { "custom_${url}":
    ensure  => file,
    path    => "/etc/nginx/sites-available/${url}_custom.inc",
  }
}
hosting::mysql {}
hosting::php {}
hosting {}
$siteurl = 'www.piedpiper.com'
site {
  type => 'custom',
  url => $siteurl,
}