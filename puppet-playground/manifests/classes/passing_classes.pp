class myClassA($foo) {

  class{$foo: }

}

class myClassB {

  notify{'b-notify':
    message => 'running here'
  }

}

class{'myClassA':
   foo => 'myClassB'
 }
