package main

import (
  "fmt"
  "Dust"
)

func main() {

  fmt.Println("Testing test...")

  var instance = &Dust.Test{}
  var result uint16 = 0

  fmt.Println("Testing duration...")

  for x := 0; x<10; x++ {
    result = instance.Duration()
    fmt.Printf("Duration: %d\n", result)
  }

  fmt.Println("Testing length...")

  for x := 0; x<10; x++ {
    result = instance.PacketLength()
    fmt.Printf("Length: %d\n", result)
  }

  fmt.Println("Testing count...")

  for x := 0; x<10; x++ {
    result = instance.PacketLength()
    fmt.Printf("Length: %d\n", result)
  }

  fmt.Println("Testing padding...")

  for x := 0; x<10; x++ {
    var bytes []byte = instance.RandomBytes(10)
    for index := range bytes {
      fmt.Printf("Random byte: %d\n", bytes[index])
    }
  }

  fmt.Println("Done.")
}
