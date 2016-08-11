/*
 * evaluation rule says that no matter where default occurs,
 * first every other option is evaluated and then if nothing
 * matches then default is evaluated if present
 */

$var = "somevar"
case $var {
  "somevar": { notice ("somevar") }
  default:   { notice ("default") }
}

case $var {
  default:   { notice ("default") }
  "somevar": { notice ("somevar") }
}
