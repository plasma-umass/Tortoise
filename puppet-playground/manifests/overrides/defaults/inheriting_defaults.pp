class parent {
  
  notify {'parent_notify': }

  Notify {
    message => 'overriding message'
  }
}

class child inherits parent {
  notify {'child_notify': }
}


class toplevel_class {
  include child
  notify {'toplevel_class_notify':}
}

include toplevel_class
