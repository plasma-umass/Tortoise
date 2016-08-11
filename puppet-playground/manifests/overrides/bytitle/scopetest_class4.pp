# This does not work as we are tying to override class outside its scope

Notify['class notify'] {
  message => 'overridden message'
}

notify {'toplevel notify':}

class someclass {

  notify{'class notify':}
}

include someclass
