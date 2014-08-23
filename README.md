Execution
=========

To run YAHUI you have to provide it with valid TLS
certificate and private key using, for instance,
command line arguments `--cert` and `--key`.

To generate key and certificate you can use following commands

    openssl genrsa -out key.pem
    openssl req -new -x509 -key key.pem -out cert.pem -days 365

