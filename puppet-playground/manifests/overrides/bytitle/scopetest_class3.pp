# This works

Notify['toplevel notify'] {
  message => 'overridden message'
}

notify {'toplevel notify':}

class someclass {

  notify{'class notify':}
}

include someclass
