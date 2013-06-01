package main


import (
  "runtime"
	"crypto/sha256"
	"fmt"
	"log"
	"net"
  "strconv"
)


func verify(message string, nonce string) bool {
	hasher := sha256.New()
	
	hasher.Write([]byte(message+nonce))
	return hasher.Sum(nil)[31] == 0
}

func doWork(message string) (string) {

  nonce := int64(0)
	stringNonce := strconv.FormatInt(nonce, 16)

	for !verify(message, stringNonce) {
		nonce += 1
    stringNonce = strconv.FormatInt(nonce, 16)
	}
	
	return stringNonce
}

func handleServerConnection(c net.Conn) {
	hash := make([]byte, 1000)

	fmt.Fprint(c, "ok\n")

	n, _ := c.Read(hash)
	castHash := string(hash[:n])
	fmt.Fprint(c, castHash+":"+doWork(castHash))
	c.Close()

}
  
func main() {
  
  runtime.GOMAXPROCS(runtime.NumCPU())
	// Listen on TCP port 2000 on all interfaces.

	l, err := net.Listen("tcp", ":1337")
	if err != nil {
		log.Fatal(err)
	}

for {
		// Wait for a connection.
		conn, err := l.Accept()
		if err != nil {
			log.Fatal(err)
		}

		go handleServerConnection(conn)
	}
}

