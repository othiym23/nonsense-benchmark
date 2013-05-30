package com.nr.nonsense_benchmark;

import java.security.MessageDigest;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

public class NonceServerHandler extends SimpleChannelUpstreamHandler {
	@Override
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) {
		e.getChannel().write("ok\n");
	}
	
	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception {
		String challenge = (String) e.getMessage();
		String candidate = null;

		for (int id=0 ;; id++) {
			candidate = Integer.toHexString(id);
			if (verify(challenge, candidate))
				break;
		}

		e.getChannel().write(challenge + ":" + candidate);
	}

	private boolean verify(String input, String nonce) throws Exception {
		MessageDigest md = MessageDigest.getInstance("SHA-256");

		md.update(input.getBytes());
		md.update(nonce.getBytes());
		byte[] digest = md.digest();

		return (digest[digest.length - 1] == 0) ? true : false;
	}
}
