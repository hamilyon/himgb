withSqliteConn ":memory:" $ runSqlConn $ (                             
        (runMigration migrateAll) >>                                             
        (insert $ Person "Michael" "Snoyman" 26)

Hello everyone, I have dificulty using hoogle as offline search engine for functions matching type signatures. How do I generate hoogle database so that it searches newlly installed pachages and any source code, e.g. my project. Can my local hoogle do this for me?
