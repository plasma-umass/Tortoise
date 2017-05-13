define irssi($flag) {
  package { 'irssi':
    ensure => $flag,
  }
}
define users::irc($key_arg, $fullname, $ensure = 'present', $username, $typ = 'rsa') {
  if $ensure == 'present' {
    user { "$username":
      name => $username,
      ensure     => present,
      comment    => $fullname,
      managehome => 'true',
      shell      => '/usr/bin/irssi',
    }

    file { "${username}_irssi_dir":
      ensure  => directory,
      path    => "/home/${username}/.irssi",
      mode    => '0755',
      owner   => $username,
      group   => $username,
    }

    file { "${username}_irssi_config":
      ensure  => present,
      path    => "/home/${username}/.irssi/config",
      content => "content omitted",
    }

    ssh_authorized_key { "${username}_key":
      ensure  => present,
      key     => $key_arg,
      type    => $typ,
      user    => $username,
    }
  } else {
    user { "$username":
      ensure     => absent,
      comment    => $fullname,
      managehome => 'true',
      shell      => '/usr/bin/irssi',
    }

    file { "${username}_irssi_dir":
      ensure  => absent,
      path    => "/home/${username}/.irssi",
      mode    => '0755',
      owner   => $username,
      group   => $username,
    }

    file { "${username}_irssi_config":
      ensure  => absent,
      path    => "/home/${username}/.irssi/config",
      content => "content omitted",
    }

    ssh_authorized_key { "${username}_key":
      ensure  => absent,
      key     => $key_arg,
      type    => $typ,
      user    => $username,
    }
  }

}
define users() {
  user { 'deployer':
    ensure     => present,
    name => 'deployer',
    managehome => 'true',
    groups     => 'wheel',
  }

  ssh_authorized_key { 'deployer_key':
    ensure  => present,
    key     => 'AAAAB3NzaC1yc2EAAAADAQABAAABAQDHB/a1L7iEH/SMUBukLpUpCQgZboOEvc+0RHMQZ0JMC4iaxzwoAbbDRUvv2T39NRXaojk3cgAQ9D9piN91jU9qwgVTTRs4smHs/A1yxvlsZVL879Q6pTBQpXFYMCEL9rSVQtHK27mEVht5SOoephKoTgA2icOqtbNFdWyb27v/CEE/k9sKI4igJsIbLzhjN9TYQf8LW8d9DvCuNbgXSYUK6iK/7w6hmAlHMXhCSs2LsvkjEqLSgCgUo0FRnUX76dGBpoDNKe6jryPKMlGZN5A73yOF1mpTSw33KJRXi99Uq1jQiQRfIgwHd5YSaX/Q+4xpdBaoAyh5+A45fQBGmT63',
    type    => 'rsa',
    user    => 'deployer',
  }

  ssh_authorized_key { 'deployer_key2':
    ensure  => present,
    key     => 'AAAAB3NzaC1yc2EAAAADAQABAAABAQC5U2M4XmaFit2AMOtrP01im9mkmizrl7heUq8KXXN+BFYLj8GMKTQSWpfb8uB7enh8KKuqhZLQ4FXAxY+j11UTDWmSAS/TMrj30YT6ZpKvKKO8S+ossqxoYaACiS2oTVVOtwkcoaP+S3uRjmH4crIOhuYiGbzGt0XLyDv9aH2J8bVWqcWw31P5NjzTAKWNhNfxFOVdRUissUPTxndgzow2KXJ51c50zWMM97rufseznqvTOFMrcHag7QEcxe1LCKw/5RkUD8exAn336Hpcq57ipJvVb5jU6Yz21QIGuQgsJ6c07BASGGnDqQljO4NCVdR/ftszvQ56s8gUPqe/bkUx',
    type    => 'rsa',
    user    => 'deployer',
  }

  ssh_authorized_key { 'nfisher_key2':
    ensure  => present,
    key     => 'AAAAB3NzaC1yc2EAAAADAQABAAABAQC5U2M4XmaFit2AMOtrP01im9mkmizrl7heUq8KXXN+BFYLj8GMKTQSWpfb8uB7enh8KKuqhZLQ4FXAxY+j11UTDWmSAS/TMrj30YT6ZpKvKKO8S+ossqxoYaACiS2oTVVOtwkcoaP+S3uRjmH4crIOhuYiGbzGt0XLyDv9aH2J8bVWqcWw31P5NjzTAKWNhNfxFOVdRUissUPTxndgzow2KXJ51c50zWMM97rufseznqvTOFMrcHag7QEcxe1LCKw/5RkUD8exAn336Hpcq57ipJvVb5jU6Yz21QIGuQgsJ6c07BASGGnDqQljO4NCVdR/ftszvQ56s8gUPqe/bkUx',
    type    => 'rsa',
    user    => 'nfisher',
  }

  # The user used to provision the users from the bouncer app
  user { 'bouncerprovisioner':
    ensure     => present,
    name => 'bouncerprovisoner',
    managehome => 'true',
    groups     => 'wheel',
  }

  ssh_authorized_key { "bouncer_provisioner_key":
    ensure => present,
    key    => "AAAAB3NzaC1yc2EAAAADAQABAAABAQDLT2Z1kNmeNbfXWDGtwei4TOl/tgW8RuyEp8FVsjkoVNfqNSUOEFhyYekjh/y5TYC95i6kZrBvKIsXO9TCmQ0kxRrhLwvwMMXLAF8QTs60bote6ExRL1pSNwmYP92wUpnJ7o5zMSUH9Pm3HKeAMSQ6sLZYNZ9VKtU07/zFbQfYKVBVd1pRjr/atpJ0Z9qkiYQbzqLyQUoKCQvdastsk2VHzgXdYnErhYH0E+Bg/1MqEVUZ/VpYirRe0FiKXzdtRq1O/cYzgOHtq1rNCcr/jzOGqHD4FsCJ29Jamksk7jfNC0wvUT0uPdkO0gDm3gMU3gCVTO3BJn0kTSFNkBNm9qC7",
    type   => "rsa",
    user   => "bouncerprovisioner",
  }

  $nfisher_ensure = "present"
  users::irc {
    ensure => $nfisher_ensure,
    username => 'nfisher',
    fullname => 'Nathan Fisher',
    key_arg => 'AAAAB3NzaC1yc2EAAAADAQABAAABAQDHB/a1L7iEH/SMUBukLpUpCQgZboOEvc+0RHMQZ0JMC4iaxzwoAbbDRUvv2T39NRXaojk3cgAQ9D9piN91jU9qwgVTTRs4smHs/A1yxvlsZVL879Q6pTBQpXFYMCEL9rSVQtHK27mEVht5SOoephKoTgA2icOqtbNFdWyb27v/CEE/k9sKI4igJsIbLzhjN9TYQf8LW8d9DvCuNbgXSYUK6iK/7w6hmAlHMXhCSs2LsvkjEqLSgCgUo0FRnUX76dGBpoDNKe6jryPKMlGZN5A73yOF1mpTSw33KJRXi99Uq1jQiQRfIgwHd5YSaX/Q+4xpdBaoAyh5+A45fQBGmT63'
  }
}
irssi { flag => 'installed' }
users {}