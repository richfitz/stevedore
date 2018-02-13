#!/usr/bin/env bash
TOKEN=$(cat /vagrant/worker_token)
docker swarm join --token "$TOKEN" 192.168.99.100:2377
