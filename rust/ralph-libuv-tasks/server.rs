extern mod std;

use sha256;

fn main() {
  type ConnectMsg = (std::net::tcp::TcpNewConnection, comm::SharedChan<Option<std::net::tcp::TcpErrData>>);
  
  let (port, chan): (Port<ConnectMsg>, Chan<ConnectMsg>) = stream();

  do task::spawn {
    let (conn, kill_chan) = port.recv();
    info!("Fixin to accept");
    match std::net::tcp::accept(conn) {
      Ok(socket) => {
        info!("Accepted connection");
        let socket_buf = std::net::tcp::socket_buf(socket);
        let socket_write = @socket_buf as @io::WriterUtil;
        socket_write.write_line("ok");
      },
      Err(_) => {
        info!("Failed to accept connection");
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
    Err(_) => info!("wut"),
    Ok(_) => info!("done")
  }
}
