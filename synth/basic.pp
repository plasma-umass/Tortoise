# "/foo" -> nil, "/bar" -> dir

$x = "/foo"
file {$x:
  ensure => directory 
}
