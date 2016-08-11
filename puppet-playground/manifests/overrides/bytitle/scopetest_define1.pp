notify {'toplevel notify':}

define somedefine {

# This fails as the toplevel notify is not in its scope
  Notify['toplevel notify'] {
    message => 'overridden message'
  }

  notify {$name:}
}

somedefine {'some define':}
