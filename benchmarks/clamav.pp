# From https://github.com/mjhas/clamav

package { 'amavisd-new':
  ensure => latest
}

define clamav(
  $milter='false',
  $amavis='true',
) {
  if $amavis == 'true' {
    user {'clamav':
      name => 'clamav',
    }
  }
  package {'clamav-daemon':
    ensure  => latest
  }
  package {'clamav-freshclam':
    ensure  => latest,
  }
  if $milter == 'true' {
    package {'clamav-milter':
      ensure  => latest,
    }
  }
}

$amavisflag = 'true'
$milterflag = 'false'
clamav {
  amavis => $amavisflag,
  milter => $milterflag
}