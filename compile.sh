mkdir -p ebin
erlc -I /lib/ejabberd-*/include -o ebin src/mod_register_announce.erl && 
cp ebin/mod_register_announce.beam /lib/ejabberd-*/ebin
cp /etc/ejabberd/ejabberd.yml /etc/ejabberd/ejabberd.yml.bk
sed -i sed -i "s/mod_restful:/mod_register_announce: {}\n  mod_restful:/" /etc/ejabberd/ejabberd.yml && 
su - ejabberd -c "ejabberdctl restart"
