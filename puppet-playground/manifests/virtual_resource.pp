@file { 'testfile':
  path    => '/home/vagrant/from-master',
          ensure  => present,
          mode    => 0600,
          content => "Created by the master\n"
}

class database {
  realize File['testfile']
}

class webserver {
  realize File['testfile']
}

node 'agent' {

  /*
   * Though both classes realize same resource, it gets instantiated only once.
   * This could also be achieved by having single resource classes and including
   * them. Modelling as virtual resource allows the capability to select resource
   * groups based on their names and/or attributes
   */

  include database
  include webserver
}
