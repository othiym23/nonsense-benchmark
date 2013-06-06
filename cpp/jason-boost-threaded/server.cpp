#include <cstdlib>
#include <iostream>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>
#include <boost/shared_ptr.hpp>

#ifdef __APPLE__
  #include <CommonCrypto/CommonDigest.h>
  #define NS_SHA_CTX    CC_SHA256_CTX
  #define NS_SHA_INIT   CC_SHA256_Init
  #define NS_SHA_UPDATE CC_SHA256_Update
  #define NS_SHA_FINAL(CTX, BUF)  CC_SHA256_Final((BUF), (CTX))
  #define NS_SHA_LENGTH CC_SHA256_DIGEST_LENGTH
#else /* !__APPLE__ */
  #include <openssl/sha.h>
  #define NS_SHA_CTX              SHA256_CTX
  #define NS_SHA_INIT             SHA256_Init
  #define NS_SHA_UPDATE           SHA256_Update
  #define NS_SHA_FINAL(CTX, BUF)  SHA256_Final((CTX), (CTX))
  #define NS_SHA_LENGTH           32
#endif

#define HASH_STR_SIZE 64
#define OK_STR "ok\n"
#define OK_STR_SIZE 3
#define HEX_CRIB "fedcba9876543210123456789abcdef"

// We don't need interruptions and they're less efficient.
#define BOOST_THREAD_DONT_PROVIDE_INTERRUPTIONS

using boost::asio::ip::tcp;
using namespace std;

typedef boost::shared_ptr<boost::asio::io_service> io_service_ptr;
typedef boost::shared_ptr<boost::asio::io_service::work> work_ptr;

NS_SHA_CTX context;

class session {
public:
  session(boost::asio::io_service& io_service)
  : socket_(io_service)
  {
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
    nonce = 0;

    data_[HASH_STR_SIZE] = ':';
    nonce_str = (char*)data_ + HASH_STR_SIZE + 1;

    memcpy(&my_context, &context, sizeof(NS_SHA_CTX));
    NS_SHA_UPDATE(&my_context, data_, HASH_STR_SIZE);

    while(true) {
      memcpy(&test_context, &my_context, sizeof(NS_SHA_CTX));

      nonce_str_len = itoa_16(nonce, nonce_str);

      NS_SHA_UPDATE(&test_context, nonce_str, nonce_str_len);
      NS_SHA_FINAL(&test_context, result);

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

  NS_SHA_CTX my_context;
  NS_SHA_CTX test_context;
  unsigned char result[HASH_STR_SIZE/2];
  char *nonce_str;
  unsigned int nonce;
  unsigned int nonce_str_len;
};

class io_service_pool {
public:
  io_service_pool(std::size_t pool_size)
  : next_io_service_(0)
  {
    if (pool_size == 0)
      throw std::runtime_error("io_service_pool size is 0");

    pool_size_ = pool_size;

    // Give all the io_services work to do so that their run() functions will
    // not exit until they are explicitly stopped.
    for (std::size_t i = 0; i < pool_size; ++i) {
      io_service_ptr io_service(new boost::asio::io_service);
      work_ptr work(new boost::asio::io_service::work(*io_service));
      io_services_.push_back(io_service);
      work_.push_back(work);
    }
  }

  void run() {
    // Create a pool of threads to run all of the io_services.
    std::vector<boost::shared_ptr<boost::thread> > threads;
    for (std::size_t i = 0; i < io_services_.size(); ++i) {
      boost::shared_ptr<boost::thread> thread(new boost::thread(
            boost::bind(&boost::asio::io_service::run, io_services_[i])));
      threads.push_back(thread);
    }

    // Wait for all threads in the pool to exit.
    for (std::size_t i = 0; i < threads.size(); ++i)
      threads[i]->join();
  }

  boost::asio::io_service& get_io_service() {
    // Use a round-robin scheme to choose the next io_service to use.
    boost::asio::io_service& io_service = *io_services_[next_io_service_];
    ++next_io_service_;
    if (next_io_service_ == pool_size_)
      next_io_service_ = 0;
    return io_service;
  }

private:
  std::vector<io_service_ptr> io_services_;
  std::vector<work_ptr> work_;
  std::size_t next_io_service_;
  std::size_t pool_size_;
};

class server {
public:
  server(const std::string& address, const std::string& port, std::size_t io_service_pool_size)
  : io_service_pool_(io_service_pool_size),
    acceptor_(io_service_pool_.get_io_service())
  {
    boost::asio::ip::tcp::resolver resolver(acceptor_.get_io_service());
    boost::asio::ip::tcp::resolver::query query(address, port);
    boost::asio::ip::tcp::endpoint endpoint = *resolver.resolve(query);
    acceptor_.open(endpoint.protocol());
    acceptor_.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true));
    acceptor_.bind(endpoint);
    acceptor_.listen();

    start_accept();
  }

  void run() {
    io_service_pool_.run();
  }

private:
  void start_accept() {
    new_session = new session(io_service_pool_.get_io_service());
    acceptor_.async_accept(new_session->socket(), boost::bind(&server::handle_accept, this, boost::asio::placeholders::error));
  }

  void handle_accept(const boost::system::error_code& error) {
    if (!error) {
      new_session->start();
    } else {
      delete new_session;
    }

    start_accept();
  }

  session* new_session;
  io_service_pool io_service_pool_;
  tcp::acceptor acceptor_;
};

int main(int argc, char* argv[]) {
  unsigned cores = boost::thread::hardware_concurrency() / 2;

  printf("detected %i cores\n", cores);

  NS_SHA_INIT(&context);

  try {
    server s("0", "1337", cores);

    s.run();
  } catch (std::exception& e) {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}

