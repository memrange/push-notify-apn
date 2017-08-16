# apn

Provide Apple Push Notifications using the new http2 api.

Sending a message is as simple as:

    let sandbox = True -- Development environment
        timeout = 10   -- Minutes to keep the connection open
    session <- newSession "my.key" "my.crt"
        "/etc/ssl/ca_certificates.txt" sandbox
        timeout "my.bundle.id"
    let payload = JsonAps (JsonApsMessage (Just $ JsonApsAlert "apn"
            "Hello from Haskell") Nothing Nothing Nothing) Nothing
    success <- sendMessage session "device-token" payload
    putStrLn success

# command line utility

The command line utility can be used for testing your app. Use like this:

    apn-exe -c ../apn.crt -k ../apn.key -a \
        /etc/ssl/certs/ca-certificates.crt -b your.bundle.id -s \
        -t your-token -m "Your-message"

The -s flag means "sandbox", i.e., for apps that are deployed in a
development environment.

# credentials

apn.crt and apn.key are the certificate and private key of your
APN certificate from apple. To extract them from a .p12 file,
use openssl:

    openssl pkcs12 -in mycredentials.p12 -out apn.crt -nokeys
    openssl pkcs12 -in mycredentials.p12 -nodes -out apn.key -nocerts
    
ca-certificates.crt is a truststore that contains the root certificates
that are used to verify the apn server's server certificates.
