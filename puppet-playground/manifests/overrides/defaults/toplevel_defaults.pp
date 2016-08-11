class someclass {
  notify {'someclass_notify': }
}

include someclass

Notify {
  message => 'toplevel message'
}

