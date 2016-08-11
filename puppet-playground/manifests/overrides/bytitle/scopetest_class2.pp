notify {'toplevel notify':}

class someclass {

# This fails with error as the resource is not found
  Notify['absent resource'] {
    message => 'overridden message'
  }

  notify{'class notify':}
}

include someclass
