# apn

Provide Apple Push Notifications using the new http2 api.

Sending a message is as simple as:

    let sandbox = True -- Development environment
    session <- newSession "my.key" "my.crt" "/etc/ssl/ca_certificates.txt" sandbox 10 "my.bundle.id"
    let payload = JsonAps $ JsonApsMessage (Just "Hello from Haskell") Nothing Nothing Nothing
    success <- sendMessage session "device-token" payload

