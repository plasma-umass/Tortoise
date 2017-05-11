package pup

import ShellCommands._

object GitHubBenchmark {
  def benchmark(path: String, cmds: Seq[String], optimized: Boolean = true): Long = {
    SymbolicFS.resetState()
    val manifest = PuppetParser.parseFile(path)
    val fs = FSEval.eval(manifest.labeled.compile)
    val constraints = cmds.map(parseCommand).foldRight(fs) {
      case (cmd, fs) => PupShell.updateFileSystem(cmd, fs)
    }.toConstraints.toSet -- fs.toConstraints.toSet
    Benchmark.synthTimed(manifest, constraints.toSeq, optimized)
  }

  // A path to a benchmark and a collection of shell command sequences each representing a different
  // update to the manifest.
  type Experiment = (String, Seq[Seq[String]])
  def experiment(path: String)(commandSequences: Seq[String]*) = (path, commandSequences.toSeq)
  def experiments(exps: Experiment*): Seq[Experiment] = exps.toSeq

  def runExperiment(trials: Int, optimized: Boolean = true) = {
    val exps = experiments(
      experiment("benchmarks/amavis.pp")(
        Seq("apt remove spamassassin")
      ),
      experiment("benchmarks/bind.pp")(
        Seq("mv /var/log/named /var/named/chroot/var/log/named"),
        Seq("apt remove bind9", "apt install bind9-chroot"),
        Seq("apt install bind9-chroot", "apt remove bind9")
      ),
      experiment("benchmarks/clamav.pp")(
        Seq("userdel clamav"),
        Seq("apt install clamav-milter")
      ),
      experiment("benchmarks/hosting.pp")(
        Seq("mv /www/www.piedpiper.com /www/www.piperchat.com")
        // Seq("mv /etc/nginx/sites-enabled/www.piedpiper.com /etc/nginx/sites-enabled/www.piperchat.com"),
        // Seq("mv /etc/nginx/sites-available/www.piedpiper.com /etc/nginx/sites-available/www.piperchat.com")
      ),
      experiment("benchmarks/jpa.pp")(
        Seq("apt remove unzip")
      ),
      experiment("benchmarks/logstash.pp")(
        Seq("rm /etc/logstash"),
        Seq("rm /etc/logstash/conf.d"),
        Seq("rm /usr/local/logstash"),
        Seq("rm /var/log/logstash"),
        Seq("rm /etc/init.d"),
        Seq("rm /etc/logrotate.d")
      ),
      experiment("benchmarks/monit.pp")(
        Seq("mv /etc/monit/conf.d/monit /etc/monit/conf.d/foobar"),
        Seq("chmod 600 /etc/monit/conf.d/monit"),
        Seq("mv /etc/monit/conf.d/monit /etc/monit/conf.d/foo", "chmod 600 /etc/monit/conf.d/foo"),
        Seq("chmod 600 /etc/monit/conf.d/monit", "mv /etc/monit/conf.d/monit /etc/monit/conf.d/foo")
      ),
      experiment("benchmarks/nginx.pp")(
        Seq("rm /etc/nginx/includes", "mkdir /etc/nginx/includedir"),
        Seq("rm /etc/nginx/conf.d", "mkdir /etc/nginx/confdir"),
        Seq("mv /etc/nginx/includes /etc/nginx/include.d"),
        Seq("mv /etc/nginx/conf.d /etc/nginx/confdir")
      ),
      experiment("benchmarks/ntp.pp")(
        Seq("rm /etc/logrotate.d/ntpd"),
        Seq("mv /etc/ntp.conf /etc/ntpd.conf"),
        Seq("apt remove ntp", "apt install ntpd")
      ),
      experiment("benchmarks/powerdns.pp")(
        Seq("apt remove pdns-server", "dnf install powerdns"),
        Seq("chmod 644 /etc/powerdns/pdns.d"),
        Seq("mv /etc/powerdns/pdns.d /etc/powerdns/conf.d"),
        Seq("chown arjun /etc/powerdns/pdns.d"),
        Seq("chmod 644 /etc/powerdns/pdns.d/authoritive.conf"),
        Seq("chown arjun /etc/powerdns/pdns.d/authoritive.conf"),
        Seq("apt remove pdns-server")
      ),
      experiment("benchmarks/rsyslog.pp")(
        Seq("apt remove rsyslog"),
        Seq("mv /etc/rc.d/rsyslog /etc/rc.d/syslog"),
        Seq("chown rsyslog /etc/rc.d/rsyslog"),
        Seq("rm /etc/rsyslog.d/rsyslog")
      ),
      experiment("benchmarks/xinetd.pp")(
        Seq("put /etc/xinetd.d/xinetd /usr/bin/qsync --daemon --config /etc/rsync.conf 873"),
        Seq("put /etc/xinetd.d/xinetd /usr/bin/qsync --daemon --config /etc/rsync.conf 875"),
        Seq("put /etc/xinetd.d/xinetd /usr/bin/qsync --daemon --config /etc/qsync.conf 873"),
        Seq("put /etc/xinetd.d/xinetd /usr/bin/qsync --daemon --config /etc/qsync.conf 875"),
        Seq("rm /etc/xinetd.d/xinetd")
      )
    )

    val updates = exps.toMap

    val timings = exps.map {
      case (path, cmdSequences) => path -> (cmdSequences.flatMap {
        cmds => Benchmark.trials(trials) {
          benchmark(path, cmds, optimized)
        }
      }.sum / (cmdSequences.length * trials))
    }.toMap

    val paths = exps.map(_._1)
    paths.map {
      path => {
        val name = """/([^.]*).pp""".r.findFirstMatchIn(path).get.group(1)
        val size = PuppetVisitors.size(PuppetParser.parseFile(path))
        val numUpdates = updates(path).length
        val averageTime = timings(path) / 1000000

        s"$name & $size & $numUpdates & $averageTime \\\\"
      }
    }.foldLeft("Name & Resources & Updates & Average Time (ms) \\\\") {
      case (acc, line) => s"${acc}\n$line"
    }
  }
}
