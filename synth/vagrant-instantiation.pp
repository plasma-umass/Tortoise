define g($y, $mod) {
  file {$y:
    ensure => directory,
    mode => $mod
  }
}
define f($x, $mode) {
  g {
    y => $x,
    mod => $mode
  }
}
$m = "600"
$w = "/home/vagrant/foo"
f {
  x => $w,
  mode => $m
}
$p = "600"
$v = "/home/vagrant/baz"
f {
  x => $v,
  mode => $p
}
