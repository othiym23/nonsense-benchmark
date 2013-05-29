require 'digest'
require 'openssl'

def verify(input, nonce)
  if input
    hashed = OpenSSL::Digest::SHA256.new << input + nonce
    hashed if hashed.to_s[62..64] == '00'
  end
end

str = 'e5fa44f2b31c1fb553b6021e7360d07d5d91ff5e'

id = 0
while true
  if nonce = verify(str, id.to_s(16))
    puts id.to_s(16)
    break
  else
    id += 1
  end
end
