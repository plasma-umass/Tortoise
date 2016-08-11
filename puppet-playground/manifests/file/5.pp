/*
 * Successfull application of this manifest depends on the current
 * state of path in question
 *
 * - If the path does not exist then puppet will happily create a new link
 * - If the path exist as a file then puppet has no problems in converting it into
 *   a link
 * - If the path exist as a directory then puppet refuses to convert it into a link,
 *   unless 'force' attribute is set as true
 */

file { '/vagrant/manifests/file/link':
  ensure => link,
  target => '/tmp/',
  force => true
}
