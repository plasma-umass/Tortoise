notify{'A':
  message => 'A message'
}

notify{'B':
  message => 'B message'
}

notify{'C':
  message => 'C message'
}

$x = 'a'

case $x {
  'a': {  }
  default: {  }
} #~> Notify['B']