define g($y) {
  file {$y:
    ensure => directory
  }
}

define f($x) {
  g { y => $x }
}

$w = "/foo"
f { x => $w }
$v = "/baz"
f { x => $v }
