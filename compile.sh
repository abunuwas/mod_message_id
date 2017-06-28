mkdir -p ebin
erlc -I /lib/ejabberd-*/include -pa /lib/lager-*/ebin -o ebin src/mod_message_id.erl && 
cp ebin/mod_message_id.beam /lib/ejabberd-*/ebin &&
cp /etc/ejabberd/ejabberd.yml /etc/ejabberd/ejabberd.yml.bk &&
sed -i "s/mod_restful:/mod_message_id: {}\n  mod_restful:/" /etc/ejabberd/ejabberd.yml && 
su - ejabberd -c "ejabberdctl restart"
