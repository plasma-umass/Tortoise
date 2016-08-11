define mynotify ($param1, $param2 = 'p2') {

  notify { $param1:
    message => $param2
  }
}


Mynotify['a'] {
#  param1 => 'name override',
  param2 => 'message override'
}

mynotify {'a': 
  param1 => 'p1',
#  param2 => 'p2'
}
