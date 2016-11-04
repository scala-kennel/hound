import Build._

buildSettings
publish := {}
publishLocal := {}
publishArtifact := false

lazy val houndJVM = hound.jvm
lazy val houndJS = hound.js
