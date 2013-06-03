import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

public class Server {
    private InetAddress addr;
    private int port;
    private Selector selector;
    private Map<SocketChannel,List<byte[]>> dataMap;

    public Server(InetAddress addr, int port) throws IOException {
        this.addr = addr;
        this.port = port;
        dataMap = new HashMap<SocketChannel,List<byte[]>>();
        startServer();
    }

    private void startServer() throws IOException {
        // create selector and channel
        this.selector = Selector.open();
        ServerSocketChannel serverChannel = ServerSocketChannel.open();
        serverChannel.configureBlocking(false);

        // bind to port
        InetSocketAddress listenAddr = new InetSocketAddress(this.addr, this.port);
        serverChannel.socket().bind(listenAddr);
        serverChannel.register(this.selector, SelectionKey.OP_ACCEPT);

        log("Echo server ready. Ctrl-C to stop.");

        // processing
        while (true) {
            // wait for events
            this.selector.select();

            // wakeup to work on selected keys
            Iterator keys = this.selector.selectedKeys().iterator();
            while (keys.hasNext()) {
                SelectionKey key = (SelectionKey) keys.next();

                // this is necessary to prevent the same key from coming up 
                // again the next time around.
                keys.remove();

                if (! key.isValid()) {
                    continue;
                }

                if (key.isAcceptable()) {
                    this.accept(key);
                }
                else if (key.isReadable()) {
                    this.read(key);
                }
                else if (key.isWritable()) {
                    this.write(key);
                }
            }
        }
    }

    private void accept(SelectionKey key) throws IOException {
        ServerSocketChannel serverChannel = (ServerSocketChannel) key.channel();
        SocketChannel channel = serverChannel.accept();
        channel.configureBlocking(false);

        // write welcome message
        channel.write(ByteBuffer.wrap("ok\n".getBytes()));
        
        Socket socket = channel.socket();
        SocketAddress remoteAddr = socket.getRemoteSocketAddress();
        //log("Connected to: " + remoteAddr);

        // register channel with selector for further IO
        dataMap.put(channel, new ArrayList<byte[]>());
        channel.register(this.selector, SelectionKey.OP_READ);
    }

    private void read(SelectionKey key) throws IOException {
        SocketChannel channel = (SocketChannel) key.channel();

        ByteBuffer buffer = ByteBuffer.allocate(8192);
        int numRead = -1;
        try {
            numRead = channel.read(buffer);
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        if (numRead == -1) {
            this.dataMap.remove(channel);
            Socket socket = channel.socket();
            SocketAddress remoteAddr = socket.getRemoteSocketAddress();
            //log("Connection closed by client: " + remoteAddr);
            key.cancel();
            channel.close();
            return;
        }

        byte[] data = new byte[numRead];
        System.arraycopy(buffer.array(), 0, data, 0, numRead);
        String input = new String(data);
        //log("Got: " + input);
        
        // write back to client
        doVerify(key, input);
    }

    private void write(SelectionKey key) throws IOException {
        SocketChannel channel = (SocketChannel) key.channel();
        List<byte[]> pendingData = this.dataMap.get(channel);
        Iterator<byte[]> items = pendingData.iterator();
        while (items.hasNext()) {
            byte[] item = items.next();
            items.remove();
            channel.write(ByteBuffer.wrap(item));
        }
        key.interestOps(SelectionKey.OP_READ);
    }

    private void doVerify(SelectionKey key, String input) {
        SocketChannel channel = (SocketChannel) key.channel();
        List<byte[]> pendingData = this.dataMap.get(channel);   
        int id = 1;
        while (true) {
          String nonce = Integer.toString(id, 16);
          if (verify(input, nonce) == true) {                           
            pendingData.add((input + ":" + nonce).getBytes());
            key.interestOps(SelectionKey.OP_WRITE);
            break;
          }
          id++;
        }
    }

    private static boolean verify(String input, String nonce) {
    	return (digest(input.getBytes(), nonce.getBytes())[31] == 0) ? true : false;
    }
    
    private static byte[] digest(byte[] input, byte[] nonce) {
        try {
          MessageDigest digest = MessageDigest.getInstance("SHA-256");
          digest.update(input);
          digest.update(nonce);
          return digest.digest(); 
        } catch (NoSuchAlgorithmException e) {
          throw new RuntimeException(e);
        } 
    }
    
    private static void log(String s) {
        System.out.println(s);
    }

    public static void main(String[] args) throws Exception {
        new Server(null, 1337);
    }
}