/* Create a heirarcy as depicted below but all three should be directories: dir1, file1 and file2
 * Delete file1 and file2
 * Reapply puppet manifest, it restores them as files

 * Delete the whole heirarchy
 * Reapply puppet manifest, it shall create dir1 as file and throw error for rest of them
 */

file { '/vagrant/manifests/file/dir1':
  ensure => present
}


file { '/vagrant/manifests/file/dir1/file1':
  ensure => present
}


file { '/vagrant/manifests/file/dir1/file2':
  ensure => present
}
