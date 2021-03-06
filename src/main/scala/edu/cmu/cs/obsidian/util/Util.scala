package edu.cmu.cs.obsidian.util

/**
  * Created by mcoblenz on 2/24/17.
  */
object Util {
    // Protobuf specifications are required to be in snake case, but Java and Obsidian use camel case.
    def snakeCaseToCamelCase(s: String): String = {
        // TODO
        ""
    }

    def protobufOuterClassNameForFilename(filename: String): String = {
        val lastSlash = filename.lastIndexOf("/")
        val fileWithoutPath: String = if (lastSlash >= 0) filename.substring(lastSlash + 1) else filename
        val filePrefix = fileWithoutPath.replace(".obs", "")

        filePrefix.substring(0, 1).toUpperCase(java.util.Locale.US) + filePrefix.substring(1) + "OuterClass"
    }

}
