extern mod std;

fn sha256(msg: &str) -> ~str {
  ~""
}

fn main() {
  type ConnectMsg = (std::net::tcp::TcpNewConnection, comm::SharedChan<Option<std::net::tcp::TcpErrData>>);
  
  let (port, chan): (Port<ConnectMsg>, Chan<ConnectMsg>) = stream();

  do task::spawn {
    loop {
      let (conn, kill_chan) = port.recv();
      info!("Fixin to accept");
      match std::net::tcp::accept(conn) {
        Ok(socket) => {
          info!("Accepted connection");

          socket.write(str::to_bytes("ok\n"));

          let res = socket.read(0);
          let input_str = str::from_bytes(res.get());
          info!(fmt!("Read %s", input_str));

          let nonce = ~"5a";

          socket.write(str::to_bytes(input_str + ":" + nonce));
        },
        Err(_) => {
          info!("Failed to accept connection");
        }
      }
    }
  }

  let ip = std::net_ip::v4::parse_addr("127.0.0.1");
  let task = std::uv_global_loop::get();

  match std::net::tcp::listen(ip, 1337, 1, &task,
    |_| {
      info!("Server listening");
    },
    |conn, kill_chan| {
      // To the accept task!
      chan.send((conn, kill_chan));
    }
  ) {
    Err(_) => info!("Error listening"),
    Ok(_) => info!("Done listening")
  }
}
