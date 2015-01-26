# Ensemble Protocol
The client begins by either initiating a new session or continuing an existing session. When creating a new session, the server returns the session ID for the new session. A number of data messages are then sent which include the session ID and the sequence number starting at 0. At any time, either side can send a close message to end the session. For particularly long-lived sessions, the sequence numbers may be insufficient for the number of messages to be sent. This case, an extend message can be sent to reset the sequence number to zero and get a new session ID which extends the current session.

## Starting a new session
*     Client request
  *      1 byte - command code, 0x00 New Session Request
*     Server response
  *      1 byte - command code, 0x01 New Session Okay
  *      32 bytes - new session ID

## Continuing an existing session
*     Client request
  *      1 byte - command code, 0x02 Continue Session Request
  *      32 bytes - session ID to continue
*     Server response
  *      1 byte - command code, 0x03 Continue Session Okay
  *      32 bytes - session ID to continue

## Extending a session
*     Client request
  *      1 byte - command code, 0x04 Extend Session Request
  *      32 bytes - session ID to extend
*     Server response
  *      1 byte - command code, 0x05 Extend Session Okay
  *      32 bytes - new session ID

## Closing a session
*     Request (can be client or server)
  *      1 byte - command code, 0x06 Close Session Request
  *      32 bytes - session ID to close
*     Response (other side, server or client)
  *      1 byte - command code, 0x07 Close Session Okay
  *      32 bytes - session ID to close

## Sending data
*     1 byte - command code, 0x0F Data
*     32 bytes - session ID
*     2 bytes - sequence number
*     2 bytes - length
*     Variable - data

## Errors
*     1 byte - error code, 0xF0 Too Many Sessions
*     1 byte - error code, 0xF1 Unknown Session ID
*     1 byte - error code, 0xF2 Connection Is Closed
*     1 byte - error code, 0xF3 Bad Sequence Number

# Encryption Protocol - Handshake

## Initializing a server
*     The server generates a static ECDH keypair using Elligator/Curve25519 and becomes the server’s certificate.

## Initializing a client
*     The client obtains a copy of the server’s certificate out of band, along with the server’s IP and port

## Creating a new session
*     Verify that the client has the certificate for the server
*     Create an ephemeral Elligator/Curve25519 keypair, associating it with the given server
*     Send the ephemeral public key to the server
*     Start sending random bytes to the server until the confirmation code is received

## When the server receives a connection from the client
*     Create an ephemeral Elligator/Curve25519 keypair
*     Send the ephemeral public key to the client
*     Start sending random bytes to the client until the client ephemeral public key is received
*     Receive the client’s Elligator-encrypted ephemeral public key
*     Decrypt the client’s encrypted ephemeral public key with Elligator to get the client’s Curve25519 ephemeral public key
*     Create a shared key (see below)
*     Create a server confirmation code using the shared key (see below)
*     Stop sending random bytes to the client
*     Send the server confirmation code to the client
*     Delete the ephemeral private key
*     Store the shared key, associated with the client
*     Receive and discard random bytes from the server until the client confirmation code is received
*     Verify the client confirmation code

## When the client receives a response from the server
*     Receive the server’s Elligator-encrypted ephemeral public key
*     Decrypt the server’s encrypted ephemeral public key with Elligator to get the server’s ephemeral Curve25519 public key
*     Create a shared key (see below)
*     Receive and discard random bytes from the server until the server confirmation code is received
*     Create a client confirmation code using the shared key (see below)
*     Verify the server confirmation code
*     Delete the ephemeral private key
*     Store the shared key, associated with the server
*     Stop sending random bytes to the client
*     Send the client confirmation code to the client

## Creating a shared key
*     On the server, hash with Skein-256-256 the following:
  *     32 bytes - ECDH(server’s ephemeral private key, client’s ephemeral public key)
  *      32 bytes - ECDH(server’s static private key, client’s ephemeral public key)
  *      7 or 19 bytes - server identifier (see below)
  *      32 bytes - client’s ephemeral public key
  *      32 bytes - server’s ephemeral public key
  *      4 bytes - “ntor”
*     On the client, hash with Skein-256-256 the following:
  *      32 bytes - ECDH(client’s ephemeral private key, server’s ephemeral public key)
  *      32 bytes - ECDH(client’s ephemeral private key, server’s static public key)
  *      7 or 10 bytes - server identifier (see below)
  *      32 bytes - client’s ephemeral public key
  *      32 bytes - server’s ephemeral public key
  *      4 bytes - “ntor”

## Creating a confirmation code
*     On the server, HMAC with Skein-256-256 and the shared key the following:
  *      7 or 19 bytes - server identifier (see below)
  *      32 bytes - server’s ephemeral public key
  *      32 bytes - client’s ephemeral public key
  *      4 bytes - “ntor”
  *      6 bytes - “server”
*     On the client, HMAC with Skein-256-256 and the shared key the following:
  *      7 or 19 bytes - server identifier (see below)
  *      32 bytes - server’s ephemeral public key
  *      32 bytes - client’s ephemeral public key
  *      4 bytes - “ntor”
  *      6 bytes - “client”

## Creating a server identifier
*     For an IPv4 address:
  *      1 byte - flags (see below)
  *      4 bytes - IP Address
  *      2 bytes - port
*     For an IPv6 address:
  *      1 byte - flags (see below)
  *      16 bytes - IP Address
  *      2 bytes - port

## Creating server identifier flags
*     0 - 0 for IPv4, 1 for IPv6
*     1 - 0 for TCP, 1 for UDP
*     2-7 - Reserved, set to 0

# Wire Protocol
 
The following reduces the complexity of the handshake protocol description into just what is actually output:
 
*     Client Request to Server
  *      32 bytes - client’s ephemeral public key
*     Server Response to Client
  *      32 bytes - server’s ephemeral public key
  *      32 bytes - confirmation code

# Encryption Protocol - Message Framing
 
The final result of the handshake protocol is that the client and server both have an identical shared key which can be used for encrypting messages. Once the handshake is complete, message transmission can commence. For this phase of communication, the Seattle protocol provides a message transmission protocol.
 
The message transmission protocol begins with a header which consists of a random initialization vector (IV). After the header, any number of records can be sent until such point as the session is ended. The record format consists of an encrypted length followed by an encrypted payload of the specified length. The payload contains a header, data, and a verification code. As the client and server use the same shared key, the message protocol is identical for both client and server.

# Message Framing Step-by-step

## Sending a message with data
*     If this is the first message, send the header (see below)
*     Generate flags (see below)
*     Generate a verification code for the data (see below)
*     Create a payload by concatenating the following:
  *      1 byte - flags
  *      Variable - data
  *      32 bytes - verification code
*     Encrypt the payload with the shared key to get the encrypted payload
*     Obtain the length of the payload and encode it as a 2-byte value in network byte order
*     Encrypt the length with the shared key to get the encrypted length
*     Create a record by concatenating the following:
  *      2 bytes - encrypted length
  *      Variable - encrypted payload
*     Deliver the message
 
## Sending a message without data
*     If this is the first message, send the header (see below)
*     Generate flags (see below)
*     Generate randomly sized blank data
  *      A random number of 0-valued bytes
*     Generate a blank verification code for the data
  *      32 0-valued bytes
*     Create a payload by concatenating the following:
  *      1 byte - flags
  *      Variable - blank data
  *      32 bytes - blank verification code
*     Encrypt the payload with the shared key to get the encrypted payload
*     Obtain the length of the payload and encode it as a 2-byte value in network byte order
*     Encrypt the length with the shared key to get the encrypted length
*     Create a record by concatenating the following:
  *      2 bytes - encrypted length
  *      Variable - encrypted payload
*     Deliver the message
 
## Sending the header
*     Generate a random 32-byte initialization vector (IV)
*     Deliver the IV

## Generate flags
*     0 - 0 for no data, 1 for data included
*     1-7 - Reserved, set to 0

## Generate a verification code for the data
*     HMAC with the shared key the following:
  *      2 bytes - unencrypted length
  *      1 byte - flags
  *      Variable - data

# Message Framing Wire Protocol
The following reduces the complexity of the message protocol description into just what messages are actually output. The client and server side use identical wire protocols.
*     32 bytes - IV
*     Repeating
  *      2 bytes - encrypted length
  *      Variable - encrypted payload
    *     If data
      *     1 byte - flags
      *     Variable - data
      *     32 bytes - verification code
    *     otherwise
      *     1 byte - flags
      *     Variable - blank data
      *     32 bytes - blank verification code
