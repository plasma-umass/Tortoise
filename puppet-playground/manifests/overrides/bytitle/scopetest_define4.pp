# This does not work as we are tying to override class outside its scope

$define_name = 'some define'

Notify[$define_name] {
  message => 'overridden message'
}

notify {'toplevel notify':}

define somedefine {

  notify {$name: }
}

somedefine {$define_name: }
