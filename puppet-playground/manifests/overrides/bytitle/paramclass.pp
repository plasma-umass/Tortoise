class a ($param1 = 'p1', $param2 = 'p2') {

  notify { $param1:
    message => $param2
  }
}


Class['a'] {
  param1 => 'name override',
  param2 => 'message override'
}

include a
