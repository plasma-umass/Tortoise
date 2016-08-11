node 'agent1' {

  file { 'sourcefile':
      path    => '/home/vagrant/src1',
      ensure  => present,
      mode    => 0600,
      content => "Created by the master\n"
  }

  /* Any 'exec' command is supposed to be idempotent as it can be applied multiple
   * times by puppet and is supposed to always return 0 for puppet to consider
   * its application a success.
   *
   * Subscribe works on refresh semantics which are only defined for 3 types of 
   * resources : exec, service and mount
   */
  exec { 'content update':
      command => "/usr/bin/md5sum /home/vagrant/src1 >> /home/vagrant/src1_md5s",
      subscribe => File['sourcefile']
  }
}
