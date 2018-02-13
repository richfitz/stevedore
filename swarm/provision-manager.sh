#!/usr/bin/env bash

## https://docs.docker.com/engine/security/https/#other-modes

## Keys:
rm -rf /vagrant/ssl
mkdir -p /etc/docker/ssl
cd /etc/docker/ssl

HOST="manager"
PASS="fake"
SUBJ="/CN=$HOST"

openssl genrsa -aes256 -passout pass:"$PASS" -out ca-key.pem 4096
openssl rsa -in ca-key.pem -passin pass:"$PASS" -out ca-key.pem
openssl req -new -x509 -days 365 -key ca-key.pem -sha256 -out ca.pem \
        -subj "$SUBJ"

openssl genrsa -out server-key.pem 4096
openssl req -subj "$SUBJ" -sha256 -new -key server-key.pem -out server.csr

echo "subjectAltName = DNS:$HOST,IP:192.168.99.100,IP:127.0.0.1" >> extfile.cnf
echo "extendedKeyUsage = serverAuth" >> extfile.cnf

openssl x509 -req -days 365 -sha256 -in server.csr \
        -CA ca.pem -CAkey ca-key.pem \
        -CAcreateserial -out server-cert.pem -extfile extfile.cnf

openssl genrsa -out key.pem 4096
openssl req -subj '/CN=client' -new -key key.pem -out client.csr
echo extendedKeyUsage = clientAuth >> client.cnf
openssl x509 -req -days 365 -sha256 -in client.csr \
        -CA ca.pem -CAkey ca-key.pem \
        -CAcreateserial -out cert.pem -extfile client.cnf

chmod -v 0400 ca-key.pem key.pem server-key.pem
chmod -v 0444 ca.pem server-cert.pem cert.pem

rm -f *.csr *.cnf

mkdir -p /vagrant/ssl
cp key.pem cert.pem ca.pem /vagrant/ssl

cat > /etc/docker/daemon.json <<EOF
{
  "tls": true,
  "tlsverify": true,
  "tlscacert": "/etc/docker/ssl/ca.pem",
  "tlscert": "/etc/docker/ssl/server-cert.pem",
  "tlskey": "/etc/docker/ssl/server-key.pem",
  "hosts": ["unix:///var/run/docker.sock", "tcp://0.0.0.0:2376"]
}
EOF

sed -i 's; -H fd://;;g' /lib/systemd/system/docker.service

systemctl daemon-reload
systemctl restart docker
service docker status

docker version
docker --tlsverify --tlscacert=ca.pem --tlscert=cert.pem --tlskey=key.pem \
       -H=manager:2376 version
docker --tlsverify --tlscacert=ca.pem --tlscert=cert.pem --tlskey=key.pem \
       -H=127.0.0.1:2376 version
docker --tlsverify --tlscacert=ca.pem --tlscert=cert.pem --tlskey=key.pem \
       -H=192.168.99.100:2376 version

docker swarm init --advertise-addr 192.168.99.100
docker swarm join-token worker -q > /vagrant/worker_token
