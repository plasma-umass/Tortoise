notify {'toplevel notify':}

define somedefine {

# This fails with error as the resource is not found
  Notify['absent resource'] {
    message => 'overridden message'
  }

  notify {$name:}
}

somedefine {'some define':}
