package org.spread.core.splithash

import java.io._

// A custom class-loader that keeps all loaded byte-code available for SPREAD to hash its own byte-code
object ClassLoader {
  val instance = AuthenticatedClassLoader(AuthenticatedClassLoader.getClass.getClassLoader)

  case class AuthenticatedClassLoader(parent: java.lang.ClassLoader) extends java.lang.ClassLoader(parent) {
    var bytecode: Map[Class[_], Array[Byte]] = Map()

    override def loadClass(name: String) = {
      if (!name.startsWith("java") && !name.startsWith("sun")) {
        val file = name.replace('.', File.separatorChar) + ".class"
        var is = getClass().getClassLoader().getResourceAsStream(file)

        if (is != null) {
          val buffer = new ByteArrayOutputStream()
          var data: Array[Byte] = new Array(128)
          var n = 0

          n = is.read(data, 0, data.length)

          while (n != -1) {
            buffer.write(data, 0, n)
            n = is.read(data, 0, data.length)
          }

          buffer.flush()
          var b: Array[Byte] = buffer.toByteArray

          val c: Class[_] = defineClass(name, b, 0, b.length)
          resolveClass(c)
          bytecode = bytecode + (c -> b)
          c
        }
        else super.loadClass(name)
      }
      else super.loadClass(name)
    }

    // For full crypto, we have to sign the full topologically sorted set of classes that this class depends on
    // But that would require byte-code analysis, so for now we just take the single class
    def cryptoSignClass(c: Class[_]): Array[Int] = {
      import java.nio.ByteBuffer
      import java.security.MessageDigest

      val b = bytecode.get(c).get
      val md = MessageDigest.getInstance("SHA-256")

      md.reset
      md.update(b)

      val bb = ByteBuffer.wrap(md.digest).asIntBuffer
      val ib: Array[Int] = new Array(bb.limit)
      bb.get(ib)

      ib
    }
  }
}
