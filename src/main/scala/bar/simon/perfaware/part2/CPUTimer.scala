package bar.simon.perfaware.part2

import java.lang.foreign._
import java.nio.file.Paths

object CPUTimer {
  private val loader = {
    val libPath = Paths.get("src-rs/target/release/libperfaware_rs.dylib")
    val lookup  = SymbolLookup.libraryLookup(libPath, Arena.global())

    val funcAddr = lookup.find("get_cycles").orElseThrow(() => new RuntimeException("Could not find get_cycles symbol"))

    val desc = FunctionDescriptor.of(ValueLayout.JAVA_LONG)

    Linker.nativeLinker().downcallHandle(funcAddr, desc, Linker.Option.critical(true))
  }

  def getCycles: Long =
    loader.invoke().asInstanceOf[Long]
}
