// Provide a managed dependency on chisel if -DrocketVersion="" is
// supplied on the command line.

val rocketVersion = System.getProperty("rocketVersion", "None")

libraryDependencies ++= ( if (rocketVersion != "None" ) (
    "edu.berkeley.cs" %% "rocket" % rocketVersion
) :: Nil; else Nil)
