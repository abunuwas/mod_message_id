mkdir -p ebin
erlc -d -I /lib/ejabberd-16*/include -I /lib/fast_xml-1.1.23/include -pa /home/centos/eredis/ebin -pa /lib/lager-*/ebin -I /lib/xmpp-1.1.13/include -o ebin src/mod_message_id.erl && 
cp ebin/mod_message_id.beam /lib/ejabberd-16*/ebin &&
cp /etc/ejabberd/ejabberd.yml /etc/ejabberd/ejabberd.yml.bk &&
sed -i "s/mod_restful:/mod_message_id: {}\n  mod_restful:/" /etc/ejabberd/ejabberd.yml && 
su - ejabberd -c "ejabberdctl restart"
