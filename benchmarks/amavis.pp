# From https://github.com/mjhas/amavis/tree/f59dfed91dffbe89d1f7a64bbdede605ec4a0583
# Adapted to use define types instead of classes.
# Removed service and notify since they are not file-system affecting.
# Added a spamchecker argument to amavis::config

define amavis::config(
  $spamchecker             ='true',
  $bypass_virus_checks_maps =undef,
  $bypass_spam_checks_maps  =undef,
  $final_virus_destiny      =undef,
  $final_banned_destiny     =undef,
  $final_spam_destiny       =undef,
  $final_bad_header_destiny =undef,
) {
  amavis { spamassassin => $spamchecker }

  file { '/etc/amavis/conf.d/50-user':
    ensure  => present,
    content => "content omitted",
  }

  file { '/etc/amavis/conf.d/15-content_filter_mode':
    ensure  => present,
    content => "content omitted",
  }
}

define amavis(
  $spamassassin='true'
) {
  package{ 'amavisd-new':
    ensure => present,
    alias  => 'amavis',
  }

  if $spamassassin == 'true' {
    package{ 'spamassassin':
      ensure  => present 
    }
  }
}

$spamcheck = 'true'
amavis::config {
  spamchecker => $spamcheck
}