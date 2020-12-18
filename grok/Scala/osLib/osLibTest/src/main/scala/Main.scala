object Main extends App {
  os.copy(
    os.pwd/"osLibTest"/"src"/"test"/"resources"/"foofoo.txt",
    os.pwd/"tmp"/"foofoo_copy.txt"
  )
}
