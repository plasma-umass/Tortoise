class a {
  notify { 'a class notify': }

#  Notify['b class notify'] {
#    message => 'b class message overriden from a'
#  }
}

class b {
  require a

  notify { 'b class notify': }

#  Notify['a class notify'] {
#    message => 'a class message overriden from b'
#  }
}

include b
