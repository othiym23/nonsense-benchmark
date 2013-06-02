package server;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;

public class Server {  

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

	private static boolean verify(String input, String nonce) {
		return (digest(input.getBytes(), nonce.getBytes())[31] == 0) ? true : false;
	}

	public static void main(String[] args) throws Exception {		
		ByteBuffer incomingBuffer = ByteBuffer.allocateDirect(65);
		Charset charset = Charset.defaultCharset();
		CharsetDecoder decoder = charset.newDecoder();

		Selector selector = Selector.open();
		ServerSocketChannel serverSocketChannel = ServerSocketChannel.open();

		//Configure non-blocking mode
		serverSocketChannel.configureBlocking(false);

		//Bind to the specific port number
		serverSocketChannel.bind(new InetSocketAddress(1337));

		//Register the current channel with the given selector
		serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);

		while (true) {
			selector.select();
			Iterator<SelectionKey> keys = selector.selectedKeys().iterator();

			while (keys.hasNext()) {
				SelectionKey key = (SelectionKey) keys.next();
				keys.remove();

				//Accept incoming connection
				if(key.isAcceptable()) {
					ServerSocketChannel serverChannel = (ServerSocketChannel) key.channel();
					SocketChannel socketChannel = serverChannel.accept();
					socketChannel.configureBlocking(false);
					socketChannel.register(selector, SelectionKey.OP_READ);
					socketChannel.write(ByteBuffer.wrap("ok\n".getBytes()));
				}

				if (key.isReadable()) {
					SocketChannel socketChannel = (SocketChannel) key.channel();
					incomingBuffer.clear();
					socketChannel.read(incomingBuffer);
					incomingBuffer.flip();
					
					final String input = decoder.decode(incomingBuffer).toString();
					
					int id = 1;

					while (true) {
						String nonce = Integer.toString(id, 16);
						if (verify(input, nonce) == true) {
							ByteBuffer response = ByteBuffer.wrap((input + ":" + nonce).getBytes());
							socketChannel.write(response);
							key.cancel();
							socketChannel.close();
							break;
						}
						id++;
					}
				}
			}
		}
	}
}