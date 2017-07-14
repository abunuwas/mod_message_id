
#clone eredis
#cd eredis && ./rebar compile
#cp -r eredis /usr/local/lib/erlang/lib/
#clone xml, compile

mkdir -p ebin
erlc -d -I /lib/ejabberd-*/include \
        -I /lib/fast_xml-*/include \
        -I /home/centos/xmpp*/include \
        -pa /lib/lager-*/ebin \
        -pa /home/centos/eredis_pool/deps/eredis/ebin \
        -pa /home/centos/eredis_pool/deps/pool/ebin \
        -pa /home/centos/eredis_pool/ebin \
        -o ebin src/mod_message_id.erl && 
cp ebin/mod_message_id.beam /lib/ejabberd-16*/ebin &&
cp /etc/ejabberd/ejabberd.yml /etc/ejabberd/ejabberd.yml.bk &&
sed -i "s/mod_restful:/mod_message_id: {}\n  mod_restful:/" /etc/ejabberd/ejabberd.yml && 
su - ejabberd -c "ejabberdctl restart"
