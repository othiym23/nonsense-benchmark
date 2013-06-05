#!/bin/bash

sudo sysctl -w net.inet.ip.portrange.hifirst=32768
sudo sysctl -w net.inet.ip.portrange.first=32768
sudo sysctl -w net.inet.tcp.msl=3000
