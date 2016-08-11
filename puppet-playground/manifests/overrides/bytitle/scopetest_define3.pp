# This works

Notify['toplevel notify'] {
  message => 'overridden message'
}

notify {'toplevel notify':}

define somedefine {

  notify {$name:}
}

somedefine {'some define': }
