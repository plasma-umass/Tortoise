define f($x) {
  file {$x:
    ensure => directory
  }
}
$y = "/foo"
f { x => $y }
