class someclass ($msg) {

  notify {'some notify':
    message => $msg
  }
}


Class {
  msg => 'overriding message'
}

class {'someclass':
}

