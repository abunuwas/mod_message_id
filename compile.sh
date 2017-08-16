#!/bin/bash

cd ~

if [ -d eredis_pool ]; then
    rm -rf eredis_pool
fi

if [ -d xmpp ]; then
    rm -rf xmpp
fi

git clone https://github.com/hiroeorz/eredis_pool.git eredis_pool
pushd eredis_pool 
make get-deps || exit 1
make || exit 1

sudo cp -r deps/eredis /usr/local/lib/erlang/lib/
sudo cp -r deps/poolboy /usr/local/lib/erlang/lib/

popd

sudo cp -r eredis_pool /usr/local/lib/erlang/lib/

git clone https://github.com/processone/xmpp.git
pushd xmpp 
make || exit 1
popd
sudo cp -r xmpp /usr/local/lib/erlang/lib/

cd mod_message_id

mkdir -p ebin
erlc -d -I /lib/ejabberd-*/include \
        -I /lib/fast_xml-*/include \
        -I /home/centos/xmpp*/include \
        -pa /lib/lager-*/ebin \
        -pa /home/centos/eredis_pool/deps/eredis/ebin \
        -pa /home/centos/eredis_pool/deps/pool/ebin \
        -pa /home/centos/eredis_pool/ebin \
        -o ebin src/mod_message_id.erl || exit 1
sudo cp ebin/mod_message_id.beam /lib/ejabberd-16*/ebin || exit 1
sudo cp /etc/ejabberd/ejabberd.yml /etc/ejabberd/ejabberd.yml.bk || exit 1
sudo sed -i "s/mod_restful:/mod_message_id: {}\n  mod_restful:/" /etc/ejabberd/ejabberd.yml || exit 1
sudo su - ejabberd -c "ejabberdctl restart"
