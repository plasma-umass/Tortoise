file {"/etc/apache2/sites-available/000-default.conf":
  content => "dummy config",
  require => Package["apache2"]
}
package{"apache2": ensure => present }
