import java.util.logging.Level;
import java.util.logging.Logger;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.channel.ExceptionEvent;

public class MinimalServerHandler extends SimpleChannelUpstreamHandler {
    
	private static final Logger logger = Logger.getLogger(MinimalServerHandler.class.getName());
	    
	@Override
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) {
		e.getChannel().write("ok\n");
	}

	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) {
		int id = 1;
		String input = (String) e.getMessage();
		while (true) {
			String nonce = Integer.toString(id, 16);
			if (verify(input, nonce) == true) {
				e.getChannel().write(input + ":" + nonce);
				return;
			}
			id++;
		}
	}

	@Override 
	public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) {
		logger.log(Level.WARNING, "Error", e.getCause());
		e.getChannel().close();
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
	
	private boolean verify(String input, String nonce) {
		return (digest(input.getBytes(), nonce.getBytes())[31] == 0) ? true : false;
	}
	
}