$myarr = ["error", "working", "unexpected"]
$myarr[3] = "you can do this?"

notify{'mynotification':
  message => $myarr[3]
}