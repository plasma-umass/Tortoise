class myClassA($foo) {

  notify{'a-not':
    message => 'in class A',
    require => $foo
  }

}

class myClassB {

  notify{'b-notify':
    message => 'running here'
  }

}

class{'myClassB':

}

class{'myClassA':
   foo => Class['myClassB']
 }
