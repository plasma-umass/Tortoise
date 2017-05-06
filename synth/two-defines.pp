$foo_mode = "present"
$bar_mode = "directory"

define foo($name) {
  file {$name:
    ensure => $foo_mode
  }
}

define bar($name) {
  file {$name:
    ensure => $bar_mode 
  }
}

$x = "/foo"
foo { name => $x }
$y = "/bar"
bar { name => $y }
