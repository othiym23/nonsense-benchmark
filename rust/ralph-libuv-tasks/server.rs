extern mod std;
pub use core::libc::*;

fn sha256(msg: &str) -> ~str {
  unsafe {
    let s = ~"echo " + msg + " | shasum -a 256 | cut -f 1 -d ' '";
    info!(s);
    let file = str::as_c_str(s, { |cmd|
      str::as_c_str("r", { |mode|
        popen(cmd, mode)
      })
    });
    let freader = (@file as @Reader);
    let mut bytes : ~[u8] = ~[];
    loop {
      let byte : int = freader.read_byte();
      if freader.eof() { break }
      info!(fmt!("%?", byte as char));
      vec::push(bytes, byte as u8);
    }
    info!(fmt!("%?", bytes));
  }
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
          info!(sha256(input_str + nonce));

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
