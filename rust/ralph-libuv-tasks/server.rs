extern mod std;
pub use core::libc::*;

fn sha256(msg: &str) -> ~str {
  let mut hash;
  unsafe {
    let s = ~"echo " + msg + " | shasum -a 256";
    let file = str::as_c_str(s, { |cmd|
      str::as_c_str("r", { |mode|
        popen(cmd, mode)
      })
    });
    let freader = (@file as @Reader);
    let mut bytes : ~[u8] = ~[];
    loop {
      let byte : int = freader.read_byte();
      if (byte as char) == ' ' || freader.eof() { break }
      vec::push(&mut bytes, byte as u8);
    }
    hash = str::from_bytes(bytes);
  }
  hash
}

fn work_it(msg: ~str) -> ~str {
  let mut nonce : uint = 0;
  loop {
    nonce += 1;
    let hash = sha256(msg + uint::to_str(nonce));
    if str::ends_with(hash, "00") { break }
  }
  let out : ~str = msg + ":" + uint::to_str(nonce);
  out
}

fn main() {
  type ConnectMsg = (std::net::tcp::TcpNewConnection, comm::SharedChan<Option<std::net::tcp::TcpErrData>>);
  
  let (port, chan): (Port<ConnectMsg>, Chan<ConnectMsg>) = stream();

  do task::spawn {
    let mut conn_count : uint = 0;
    loop {
      let (conn, _) = port.recv();
      conn_count += 1;
      match std::net::tcp::accept(conn) {
        Ok(socket) => {
          info!(fmt!("Accepted connection %u", conn_count));

          socket.write(str::to_bytes("ok\n"));

          let res = socket.read(0);
          let input_str : ~str = str::from_bytes(res.get());
          info!(fmt!("Read %s", input_str));

          let out = work_it(input_str);

          socket.write(str::to_bytes(out));
          info!(fmt!("Sent %s", out));
        },
        Err(_) => {
          info!("Failed to accept connection");
        }
      }
    }
  }

  let ip = std::net_ip::v4::parse_addr("127.0.0.1");
  let task = std::uv_global_loop::get();

  match std::net::tcp::listen(ip, 1337, 32, &task,
    |_| {
      info!("Server listening");
    },
    |conn, kill_chan| {
      // To the accept task!
      info!("Passing to accept");
      chan.send((conn, kill_chan));
    }
  ) {
    Err(_) => info!("Error listening"),
    Ok(_) => info!("Done listening")
  }
}
