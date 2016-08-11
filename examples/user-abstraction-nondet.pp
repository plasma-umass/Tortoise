define myuser($title) {
  user {"$title":
    ensure => present,
    managehome => true
  }
  file {"/home/${title}/.vimrc":
    content => "syntax on"
  }
}
myuser {"alice": }
myuser {"carol": }
