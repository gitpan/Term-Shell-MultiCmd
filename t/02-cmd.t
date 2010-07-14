#!perl

use Test;


BEGIN { plan tests => 1}

use Term::Shell::MultiCmd ;
ok( Term::Shell::MultiCmd
    -> new ()
    -> populate ('return true' => { exec => sub { 1 }} )
    -> cmd      ('return true' )
  )
