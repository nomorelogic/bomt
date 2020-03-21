
echo "- - - | TBomt_Auth_Service    - [ get   ] autenticazione / login        | - - - - - - - - - - - - - - -"
./bomt_test_console -s autenticazione/login -m get -p "usr=marcello;pwd=pippo555;env=DDNRT"

echo "- - - | TBomt_SysConn_Service - [ get   ] sistema        / test         | - - - - - - - - - - - - - - -"
./bomt_test_console -s sistema/test -m get -p "tok=d67fdc3f6e6510182e993fe0b0a55611;host=127.0.0.1;name=DdnRt_MsSql"

echo "- - - | TBomt_SysConn_Service - [ get  ] sistema         / connessione  | - - - - - - - - - - - - - - -"
./bomt_test_console -s sistema/connessione -m get -p "tok=d67fdc3f6e6510182e993fe0b0a55611;host=127.0.0.1;name=DdnRt_MsSql"

echo "- - - | TBomt_SysConn_Service - [ post  ] sistema        / connessione  | - - - - - - - - - - - - - - -"
./bomt_test_console -s sistema/connessione -m post -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=127.0.0.1;ConnectionType=MsSql;DatabaseName=AHRDDNRETAIL;UserName=sa;Password=@dp55SQL"

echo "- - - | TBomt_SysConn_Service - [ patch ] sistema        / connessione  | - - - - - - - - - - - - - - -"
./bomt_test_console -s sistema/connessione -m patch -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=sconosciuto0;name=DdnRt_MsSql;ConnectionType=sconosciuto1;DatabaseName=sconosciuto2;UserName=sconosciuto3;Password=sconosciuto4"

echo "- - - | TBomt_SysConn_Service - [ patch ] sistema        / connessione  | - - - - - - - - - - - - - - -"
./bomt_test_console -s sistema/connessione -m patch -p "tok=d67fdc3f6e6510182e993fe0b0a55611;name=DdnRt_MsSql;HostName=127.0.0.1;ConnectionType=MsSql;DatabaseName=AHRDDNRETAIL;UserName=sa;Password=@dp55SQL"
