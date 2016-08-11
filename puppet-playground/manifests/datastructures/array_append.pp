$myarr = ["error", "working", "unexpected"]
$myarr += "you can do this?"

notify{'mynotification':
  message => $myarr
}