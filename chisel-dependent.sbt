// Provide a managed dependency on chisel if -DchiselVersion="" is
// supplied on the command line.

val chiselVersion_d = System.getProperty("chiselVersion", "None")

libraryDependencies ++= ( if (chiselVersion_d != "None" ) (
    "edu.berkeley.cs" %% "chisel" % chiselVersion_d
) :: Nil; else Nil)
