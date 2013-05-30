#include <cstdlib>
#include <iostream>
#include <boost/bind.hpp>
#include <boost/asio.hpp>

#include <openssl/sha.h>

#define HASH_STR_SIZE 64
#define OK_STR "ok\n"
#define OK_STR_SIZE 3
#define HEX_CRIB "fedcba9876543210123456789abcdef"

using boost::asio::ip::tcp;

class session {
public:
  session(boost::asio::io_service& io_service) : socket_(io_service) {
  }

  tcp::socket& socket() {
    return socket_;
  }

  void start() {
    boost::asio::async_write(socket_, boost::asio::buffer(OK_STR, OK_STR_SIZE), boost::bind(&session::handle_write, this, boost::asio::placeholders::error));

    socket_.async_receive(boost::asio::buffer(data_, max_length), boost::bind(&session::handle_read, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred));
  }

private:
  // Elegant algo courtesy of http://www.jb.man.ac.uk/~slowe/cpp/itoa.html
	int itoa_16(unsigned int value, char* result) {
		char* ptr = result, tmp_char;
		unsigned int tmp_value, len = 0;

		do {
			tmp_value = value;
			value /= 16;
			*ptr++ = HEX_CRIB[15 + (tmp_value - value * 16)];
      len++;
		} while ( value );

		return len;
	}

  void handle_read(const boost::system::error_code& error, size_t bytes_transferred) {
    unsigned char result[HASH_STR_SIZE/2];
    unsigned int nonce = 0, nonce_str_len = 0;
    char *nonce_str;

    SHA256_CTX context;

    data_[HASH_STR_SIZE] = ':';
    nonce_str = (char*)data_ + HASH_STR_SIZE + 1;

    while(true) {
      nonce_str_len = itoa_16(nonce, nonce_str);

      SHA256_Init(&context);
      SHA256_Update(&context, data_, bytes_transferred);
      SHA256_Update(&context, nonce_str, nonce_str_len);
      SHA256_Final(result, &context);

      if(result[31] == 0)
        break;

      nonce++;
    }

    boost::asio::async_write(socket_, boost::asio::buffer(data_, HASH_STR_SIZE + nonce_str_len + 1), boost::bind(&session::handle_write, this, boost::asio::placeholders::error));
    
    socket_.close();
  }

  void handle_write(const boost::system::error_code& error) {
    if(error)
      delete this;
  }

  tcp::socket socket_;
  enum { max_length = 1024 };
  unsigned char data_[max_length];
};

class server {
public:
  server(boost::asio::io_service& io_service, short port) : io_service_(io_service), acceptor_(io_service, tcp::endpoint(tcp::v4(), port)) {
    start_accept();
  }

private:
  void start_accept() {
    session* new_session = new session(io_service_);
    acceptor_.async_accept(new_session->socket(), boost::bind(&server::handle_accept, this, new_session, boost::asio::placeholders::error));
  }

  void handle_accept(session* new_session, const boost::system::error_code& error) {
    if (!error) {
      new_session->start();
    } else {
      delete new_session;
    }

    start_accept();
  }

  boost::asio::io_service& io_service_;
  tcp::acceptor acceptor_;
};

int main(int argc, char* argv[]) {
  try {
    boost::asio::io_service io_service;

    server s(io_service, 1337);

    io_service.run();
  } catch (std::exception& e) {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}

