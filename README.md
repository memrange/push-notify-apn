# push-notify-apn

push-notify-apn is a library and command line utility that can be used to send
push notifications to mobile devices running iOS. Push notifications are small
messages that can be sent to apps on smart phones and tablets
without the need to keep open a long lived TCP connection per app, dramatically
reducing the power consumption in standby mode.

The library is still in an experimental state. Bug and success reports
as well as feature and pull requests are very welcome.

Sending a message is as simple as:

    let sandbox     = True  -- Development environment
        maxParallel = 10    -- Number of parallel connections to
                            -- the APN Servers
        useJwt      = False -- No message bearer Token
    session <- newSession (Just "my.key") (Just "my.crt")
        (Just "/etc/ssl/ca_certificates.txt") useJwt sandbox
        maxParallel "my.bundle.id"
    let payload = alertMessage "Title" "Hello From Haskell"
        message = newMessage payload
        token   = base16EncodedToken "the-token"
    success <- sendMessage session token None payload
    print success

# command line utility

The command line utility can be used for testing your app. Use like this:

    sendapn -c ../apn.crt -k ../apn.key -a \
        /etc/ssl/certs/ca-certificates.crt -b your.bundle.id -s \
        -t your-token -m "Your-message"

The -s flag means "sandbox", i.e., for apps that are deployed in a
development environment.

You can also use an interactive mode, where messages are read from
stdin in this format:

    token:sound:title:message
    
To use, invoke like this:

    stack exec -- sendapn -k ~/greaselapn.key -c ~/greaselapn.crt -a /etc/ssl/cert.pem -b org.hcesperer.greasel -s -i
    
Do remove the -s flag when using the production instead of the sandbox environment.

# credentials

apn.crt and apn.key are the certificate and private key of your
APN certificate from apple. To extract them from a .p12 file,
use openssl:

    openssl pkcs12 -in mycredentials.p12 -out apn.crt -nokeys
    openssl pkcs12 -in mycredentials.p12 -nodes -out apn.key -nocerts
    
ca-certificates.crt is a truststore that contains the root certificates
that are used to verify the apn server's server certificates.
