$myarr = ["error", "working", "unexpected"]
$elt = $myarr[1]

notify{'mynotification':
  message => $elt
}