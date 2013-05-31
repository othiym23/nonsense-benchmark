require 'digest'
require 'openssl'

def verify(input, nonce)
  if input
    hashed = OpenSSL::Digest::SHA256.new << input + nonce
    hashed if hashed.to_s[60..64] == '0000'
  end
end

str = 'hello'

id = 0
while true
  if nonce = verify(str, id.to_s(16))
    puts id.to_s(16)
    break
  else
    id += 1
  end
end
