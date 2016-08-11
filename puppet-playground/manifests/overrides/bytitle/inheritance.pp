class parent {
  notify {'parent class': }

#  Notify['child class'] {
#    message => 'child class overridden message'
#  }
}

class child inherits parent {

  notify {'child class': }
  
  Notify['parent class'] {
    message => 'parent class overridden message'
  }

}

include child 
