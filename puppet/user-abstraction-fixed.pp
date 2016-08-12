define myuser($title) {
  user {"$title":
    ensure => present,
    managehome => true
  }
  file {"/home/${title}/.vimrc":
    content => "syntax on"
  }
  User["$title"] -> File["/home/${title}/.vimrc"]
}
myuser {"alice": }
myuser {"carol": }
