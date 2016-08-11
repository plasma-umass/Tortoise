# It is interesting that this does not loop. It is not a naive, C-style
# include.
class myClassA($foo) {

  include $foo
  notify{'a-notify':
    message => 'in here'
  }

}

class{'myClassA':
   foo => 'myClassA'
 }
