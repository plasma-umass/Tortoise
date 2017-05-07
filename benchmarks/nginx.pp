$nginxversion = '1.0.0' 
$nginx_user = undef
$::nginx_worker_processes = undef
$::nginx_worker_connections = undef

define nginx($includes, $conf) {
  $real_nginx_user = 'www-data'
  $real_nginx_worker_processes = '1'
  $real_nginx_worker_connections = '1024'


  package { 'nginx': ensure => installed }

  file { '/etc/nginx/nginx.conf':
    ensure  => present,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
    content => "content omitted"
  }

  file { $conf:
    ensure  => directory,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
  }

  file { '/etc/nginx/ssl':
    ensure  => directory,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
  }

  file { $includes:
    ensure  => directory,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
  }

  file { '/etc/nginx/sites-available':
    ensure  => directory,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
  }

  file { '/etc/nginx/sites-enabled':
    ensure  => directory,
    mode    => '0644',
    owner   => 'root',
    group   => 'root',
  }

  # Nuke default files
  file { '/etc/nginx/fastcgi_params':
    ensure  => absent,
  }
}

$includedir = '/etc/nginx/includes'
$confdir = '/etc/nginx/conf.d'
nginx {
  includes => $includedir,
  conf => $confdir,
}