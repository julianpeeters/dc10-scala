// build info
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.13.0")

// cross
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.17.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.5")

// docs
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                      % "2.6.1")

// publish
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"                % "1.9.0")
addSbtPlugin("org.typelevel"      % "sbt-typelevel-no-publish"      % "0.7.4")